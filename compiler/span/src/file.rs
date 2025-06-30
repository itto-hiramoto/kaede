use std::{fmt::Display, hash::Hash, path::PathBuf, sync::Mutex};

use once_cell::sync::Lazy;
use slab::Slab;

static FILES: Lazy<Mutex<Slab<PathBuf>>> = Lazy::new(|| Mutex::new(Slab::new()));

// Do not derive PartialEq, Hash, etc!
// We need to compare symbols(string) as well as id
#[derive(Debug, Clone, Copy)]
pub struct FilePath {
    id: usize,
}

impl FilePath {
    pub fn path(&self) -> PathBuf {
        FILES.lock().unwrap()[self.id].clone()
    }

    pub fn dummy() -> Self {
        Self { id: usize::MAX }
    }
}

impl Default for FilePath {
    fn default() -> Self {
        Self::dummy()
    }
}

impl From<PathBuf> for FilePath {
    fn from(value: PathBuf) -> Self {
        Self {
            id: FILES.lock().unwrap().insert(value),
        }
    }
}

impl PartialEq for FilePath {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id || self.path() == other.path()
    }
}

impl Eq for FilePath {}

impl Hash for FilePath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // It is important not to include id in hash!
        FILES.lock().unwrap()[self.id].hash(state);
    }
}

impl Display for FilePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path().as_os_str().to_string_lossy())
    }
}
