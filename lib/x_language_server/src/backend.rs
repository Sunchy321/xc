use std::{
    collections::HashMap,
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
    time::Duration,
};

use tokio::sync::Mutex;
use tower_lsp_server::{
    Client, LanguageServer,
    jsonrpc::Result,
    ls_types::{
        Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, Hover, HoverContents, HoverParams, HoverProviderCapability,
        InitializeParams, InitializeResult, MarkupContent, MarkupKind, MessageType, NumberOrString,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
    },
};
use xc_check::{
    CheckDiagnostic, DiagnosticSeverity as XDiagnosticSeverity, TypeInfo, analyze,
    parse_program_with_diagnostics,
};
use xc_span::{create_session_globals_then, source_map::Filename};

use crate::position::{position_to_byte_offset, span_to_range};

#[derive(Clone)]
struct Document {
    version: i32,
    text: String,
    generation: u64,
    type_info: TypeInfo,
}

struct Analysis {
    diagnostic: Option<CheckDiagnostic>,
    type_info: TypeInfo,
}

pub(crate) struct Backend {
    client: Client,
    documents: Arc<Mutex<HashMap<Uri, Document>>>,
    next_generation: AtomicU64,
}

impl Backend {
    pub(crate) fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(Mutex::new(HashMap::new())),
            next_generation: AtomicU64::new(1),
        }
    }

    fn next_generation(&self) -> u64 {
        self.next_generation.fetch_add(1, Ordering::Relaxed)
    }

    async fn schedule_analysis(
        &self,
        uri: Uri,
        version: i32,
        generation: u64,
        text: String,
        delay: Duration,
    ) {
        let documents = Arc::clone(&self.documents);
        let client = self.client.clone();
        tokio::spawn(async move {
            if !delay.is_zero() {
                tokio::time::sleep(delay).await;
            }

            {
                let documents = documents.lock().await;
                if !documents
                    .get(&uri)
                    .is_some_and(|document| document.generation == generation)
                {
                    return;
                }
            }

            let analysis_uri = uri.clone();
            let source = text.clone();
            let analysis =
                tokio::task::spawn_blocking(move || analyze_document(analysis_uri, text)).await;
            let analysis = match analysis {
                Ok(analysis) => analysis,
                Err(error) => {
                    client
                        .log_message(
                            MessageType::ERROR,
                            format!("X language analysis task failed: {error}"),
                        )
                        .await;
                    return;
                }
            };

            let diagnostics = analysis
                .diagnostic
                .into_iter()
                .map(|diagnostic| to_lsp_diagnostic(&source, &diagnostic))
                .collect();

            let mut documents_guard = documents.lock().await;
            let Some(document) = documents_guard.get_mut(&uri) else {
                return;
            };
            if document.generation != generation || document.version != version {
                return;
            }
            document.type_info = analysis.type_info;
            // Queue diagnostics before a close or newer change can overtake them.
            client
                .publish_diagnostics(uri, diagnostics, Some(version))
                .await;
            drop(documents_guard);
        });
    }
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: tower_lsp_server::ls_types::InitializedParams) {}

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let item = params.text_document;
        let uri = item.uri;
        let generation = self.next_generation();
        let generation = {
            let mut documents = self.documents.lock().await;
            documents.insert(
                uri.clone(),
                Document {
                    version: item.version,
                    text: item.text.clone(),
                    generation,
                    type_info: TypeInfo::default(),
                },
            );
            generation
        };
        self.schedule_analysis(uri, item.version, generation, item.text, Duration::ZERO)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(change) = params.content_changes.into_iter().last() else {
            return;
        };
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        let generation = {
            let mut documents = self.documents.lock().await;
            let Some(document) = documents.get_mut(&uri) else {
                return;
            };
            if version <= document.version {
                return;
            }
            document.version = version;
            document.text = change.text.clone();
            document.generation = self.next_generation();
            document.type_info = TypeInfo::default();
            document.generation
        };
        self.schedule_analysis(
            uri,
            version,
            generation,
            change.text,
            Duration::from_millis(250),
        )
        .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        let mut documents = self.documents.lock().await;
        documents.remove(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
        drop(documents);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let document_position = params.text_document_position_params;
        let documents = self.documents.lock().await;
        let Some(document) = documents.get(&document_position.text_document.uri) else {
            return Ok(None);
        };
        let offset = position_to_byte_offset(&document.text, document_position.position);
        let Some(expression) = document.type_info.expression_at(offset as u32) else {
            return Ok(None);
        };
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```x\n{}\n```", expression.type_name),
            }),
            range: Some(span_to_range(&document.text, expression.span)),
        }))
    }
}

fn analyze_document(uri: Uri, text: String) -> Analysis {
    create_session_globals_then(|| {
        let filename = Filename::Normal(uri.to_string());
        let parsed = match parse_program_with_diagnostics(filename, &text) {
            Ok(parsed) => parsed,
            Err(diagnostic) => {
                return Analysis {
                    diagnostic: Some(diagnostic),
                    type_info: TypeInfo::default(),
                };
            }
        };
        let result = analyze(parsed);
        Analysis {
            diagnostic: result.result.err(),
            type_info: result.type_info,
        }
    })
}

fn to_lsp_diagnostic(source: &str, diagnostic: &CheckDiagnostic) -> Diagnostic {
    Diagnostic {
        range: span_to_range(source, diagnostic.span),
        severity: Some(match diagnostic.severity {
            XDiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
        }),
        code: Some(NumberOrString::String(diagnostic.code.as_str().to_owned())),
        source: Some("x".to_owned()),
        message: diagnostic.message.clone(),
        ..Default::default()
    }
}
