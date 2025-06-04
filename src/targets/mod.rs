use crate::{
    ast::{Ast, NodeId},
    error::CompilationError,
    utils::ComponentStorage,
};

#[cfg(feature = "target_js")]
pub(crate) mod target_js;

pub trait Target {
    type Output;
    type Options;

    fn build(
        &mut self,
        options: Self::Options,
        ast: &Ast,
        component_storage: &ComponentStorage,
        root: NodeId,
    ) -> Result<Self::Output, CompilationError>;
}
