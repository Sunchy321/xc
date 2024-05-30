use std::rc::Rc;

use md5::Digest;
use md5::Md5;
use sha1::Sha1;
use sha2::Sha256;

use crate::source_map::Filename;
use crate::BytePos;
use crate::Pos;
use crate::RelativeBytePos;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HashAlgorithm {
    MD5,
    SHA1,
    SHA256,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourceFileHash {
    pub kind: HashAlgorithm,
    value: [u8; 32],
}

impl SourceFileHash {
    pub fn new(kind: HashAlgorithm, src: &str) -> Self {
        let mut hash = Self { kind, value: Default::default() };

        let len = hash.hash_len();
        let value = &mut hash.value[..len];
        let data = src.as_bytes();

        match kind {
            HashAlgorithm::MD5 => {
                value.copy_from_slice(&Md5::digest(data));
            },
            HashAlgorithm::SHA1 => {
                value.copy_from_slice(&Sha1::digest(data));
            },
            HashAlgorithm::SHA256 => {
                value.copy_from_slice(&Sha256::digest(data));
            }
        }

        hash
    }

    fn hash_len(&self) -> usize {
        match self.kind {
            HashAlgorithm::MD5 => 16,
            HashAlgorithm::SHA1 => 20,
            HashAlgorithm::SHA256 => 32,
        }
    }
}

pub struct SourceFile {
    pub name: Filename,
    pub src: Option<Rc<String>>,
    pub src_hash: SourceFileHash,
    pub start_pos: BytePos,
    pub src_len: RelativeBytePos,
}

#[derive(Debug)]
pub struct OffsetOverflowError;

impl SourceFile {
    pub fn new(name: Filename, src: String, hash_kind: HashAlgorithm) -> Result<Self, OffsetOverflowError> {
        let src_hash = SourceFileHash::new(hash_kind, &src);

        let src_len = src.len();
        let src_len = u32::try_from(src_len).map_err(|_| OffsetOverflowError)?;

        let file = Self {
            name,
            src: Some(Rc::new(src)),
            src_hash,
            start_pos: BytePos::from_u32(0),
            src_len: RelativeBytePos::from_u32(src_len),
        };

        Ok(file)
    }

    pub fn end_pos(&self) -> BytePos {
        self.abs_pos(self.src_len)
    }

    pub fn abs_pos(&self, pos: RelativeBytePos) -> BytePos {
        BytePos::from_u32(pos.to_u32() + self.start_pos.to_u32())
    }
}