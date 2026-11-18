use xc_check::{CheckAnalysis, analyze, parse_program};
use xc_span::{create_session_globals_then, source_map::Filename};

fn analyze_source(source: &str) -> Result<CheckAnalysis, String> {
    create_session_globals_then(|| {
        let parsed = parse_program(Filename::Normal("borrow-test".into()), source)?;
        Ok(analyze(parsed))
    })
}

fn check_source(source: &str) -> Result<(), String> {
    let analysis = analyze_source(source)?;
    analysis
        .result
        .map(|_| ())
        .map_err(|error| error.to_string())
}

#[test]
fn reference_and_dereference_have_their_target_types() {
    let source = "func first(values: int[]) -> int { let r = &values; (*r)[0] }";
    let analysis = analyze_source(source).unwrap();
    assert!(analysis.result.is_ok(), "{:?}", analysis.result);
    assert_eq!(
        analysis
            .type_info
            .type_at(source.find("&values").unwrap() as u32),
        Some("int[]&")
    );
    assert_eq!(
        analysis
            .type_info
            .type_at(source.find("*r").unwrap() as u32),
        Some("int[]")
    );
    assert_eq!(
        analysis
            .type_info
            .type_at(source.find("[0]").unwrap() as u32),
        Some("int")
    );
}

#[test]
fn mutable_reference_and_dereference_have_their_target_types() {
    let source = "func first() -> int { let mut values = [1]; let r = &mut values; (*r)[0] }";
    let analysis = analyze_source(source).unwrap();
    assert!(analysis.result.is_ok(), "{:?}", analysis.result);
    assert_eq!(
        analysis
            .type_info
            .type_at(source.find("&mut values").unwrap() as u32),
        Some("int[] mut&")
    );
}

#[test]
fn reference_to_a_temporary_has_a_fresh_reference_type() {
    let source = "func value() -> int { let r = &1; *r }";
    let analysis = analyze_source(source).unwrap();
    assert!(analysis.result.is_ok(), "{:?}", analysis.result);
    assert_eq!(
        analysis
            .type_info
            .type_at(source.find("&1").unwrap() as u32),
        Some("int&")
    );
}

#[test]
fn reference_diagnostics_keep_the_inferred_type_information() {
    let source = "func take(values: int[]) {} func bad() { let values = [1]; let r = &values; take(values); print((*r)[0]); }";
    let analysis = analyze_source(source).unwrap();
    assert!(analysis.result.is_err(), "{:?}", analysis.result);
    assert_eq!(
        analysis
            .type_info
            .type_at(source.find("&values").unwrap() as u32),
        Some("int[]&")
    );
}

#[test]
fn dereferencing_a_non_reference_is_a_type_error() {
    let error = check_source("*1;").unwrap_err();
    assert!(
        error.contains("dereference operator requires a reference"),
        "{error}"
    );
}

#[test]
fn moving_a_place_while_its_reference_is_live_is_rejected() {
    let error = check_source(
        "func take(values: int[]) {} func bad() { let values = [1]; let r = &values; take(values); print((*r)[0]); }",
    )
    .unwrap_err();
    assert!(error.contains("borrowed"), "{error}");
}

#[test]
fn a_move_after_the_references_last_use_is_allowed() {
    check_source(
        "func take(values: int[]) {} func good() { let values = [1]; let r = &values; print((*r)[0]); take(values); }",
    )
    .unwrap();
}

#[test]
fn copying_a_reference_keeps_its_origin_live_through_the_alias() {
    let error = check_source(
        "func take(values: int[]) {} func bad() { let values = [1]; let r = &values; let saved = r; print(*r); take(values); print((*saved)[0]); }",
    )
    .unwrap_err();
    assert!(error.contains("borrowed"), "{error}");
}

#[test]
fn assignment_updates_a_borrowed_place_without_ending_its_identity() {
    check_source("func good() { let mut value = 1; let r = &value; value = 2; print(*r); }")
        .unwrap();
}

#[test]
fn moved_binding_is_invalid_until_reinitialized() {
    let error =
        check_source("func bad() { let values = [1]; let saved = values; print(values[0]); }")
            .unwrap_err();
    assert!(error.contains("moved"), "{error}");
    check_source(
        "func good() { let mut values = [1]; let saved = values; values = [2]; print(values[0]); }",
    )
    .unwrap();
}

#[test]
fn copying_a_copy_type_does_not_invalidate_the_source() {
    check_source("func good() { let value = 1; let saved = value; print(value); }").unwrap();
}

#[test]
fn top_level_consumption_checks_live_references() {
    let error = check_source(
        "func take(values: int[]) {} let values = [1]; let r = &values; take(values); print((*r)[0]);",
    )
    .unwrap_err();
    assert!(error.contains("borrowed"), "{error}");
}

#[test]
fn control_flow_move_on_only_one_branch_makes_the_joined_binding_maybe_moved() {
    let error = check_source(
        "func bad(flag: bool) { let values = [1]; if flag { let saved = values; } print(values[0]); }",
    )
    .unwrap_err();
    assert!(error.contains("maybe moved"), "{error}");
}

#[test]
fn control_flow_reference_last_used_inside_a_branch_allows_a_later_move() {
    check_source(
        "func take(values: int[]) {} func good(flag: bool) { let values = [1]; let r = &values; if flag { print((*r)[0]); } take(values); }",
    )
    .unwrap();
}

#[test]
fn control_flow_reference_used_in_a_loop_is_live_across_the_back_edge() {
    check_source(
        "func take(values: int[]) {} func good() { let values = [1]; let r = &values; let mut count = 0; while count < 2 { print((*r)[0]); count = count + 1; } take(values); }",
    )
    .unwrap();
}

#[test]
fn loop_back_edge_keeps_a_reference_live_before_its_next_iteration_use() {
    let error = check_source(
        "func take(values: int[]) {} func bad() { let values = [1]; let r = &values; let mut count = 0; while count < 2 { take(values); print((*r)[0]); count = count + 1; } }",
    )
    .unwrap_err();
    assert!(error.contains("borrowed"), "{error}");
}

#[test]
fn function_summary_visible_reader_does_not_retain_a_reference() {
    check_source(
        "func inspect(r: int[]&) { print((*r)[0]); } func take(values: int[]) {} func caller() { let values = [1]; inspect(&values); take(values); }",
    )
    .unwrap();
}

#[test]
fn function_summary_returned_reference_keeps_the_argument_loan_live() {
    let error = check_source(
        "func identity(r: int[]&) -> int[]& { r } func take(values: int[]) {} func caller() { let values = [1]; let saved = identity(&values); take(values); print((*saved)[0]); }",
    )
    .unwrap_err();
    assert!(error.contains("borrowed"), "{error}");
}

#[test]
fn function_summary_recursive_calls_reach_a_fixed_point() {
    let error = check_source(
        "func first(r: int[]&) -> int[]& { second(false, r) } func second(flag: bool, r: int[]&) -> int[]& { if flag { return first(r); } r } func take(values: int[]) {} func caller() { let values = [1]; let saved = first(&values); take(values); print((*saved)[0]); }",
    )
    .unwrap_err();
    assert!(error.contains("borrowed"), "{error}");
}

#[test]
fn function_summary_visible_nonlocal_store_retains_a_reference() {
    let error = check_source(
        "let mut saved = &[1]; func retain(r: int[]&) { saved = r; } func take(values: int[]) {} func caller() { let values = [1]; retain(&values); take(values); }",
    )
    .unwrap_err();
    assert!(error.contains("borrowed"), "{error}");
}

#[test]
fn function_summary_bodyless_calls_conservatively_retain_reference_arguments() {
    let error = check_source(
        "func external(r: int[]&) -> void; func take(values: int[]) {} func caller() { let values = [1]; external(&values); take(values); }",
    )
    .unwrap_err();
    assert!(error.contains("borrowed"), "{error}");
}

#[test]
fn function_summary_bodyless_declaration_requires_an_explicit_return_type() {
    let error = check_source("func external(r: int[]&);").unwrap_err();
    assert!(
        error.contains("requires an explicit return type"),
        "{error}"
    );
}
