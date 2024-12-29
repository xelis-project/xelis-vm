use std::collections::HashMap;

use xelis_common::{block::TopoHeight, contract::ContractStorage, crypto::Hash};
use xelis_vm::Constant;

// pub struct MockStorage {
//     pub data: HashMap<Constant, Constant>
// }

// impl ContractStorage for MockStorage {
//     fn load(&mut self, _: &Hash, key: Constant, _: TopoHeight) -> Result<Option<Constant>, anyhow::Error> {
//         Ok(self.data.get(&key).cloned())
//     }

//     fn has(&self, _: &Hash, key: Constant, _: TopoHeight) -> Result<bool, anyhow::Error> {
//         Ok(self.data.contains_key(&key))
//     }
// }