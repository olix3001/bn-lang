// Cache and runtime.
const __bn_module_cache__ = {};

function __bn_require__(moduleId) {
  const cachedModule = __bn_module_cache__[moduleId];
  if (cachedModule !== undefined) return cachedModule.exports;

  const module = (__bn_module_cache__[moduleId] = { exports: {} });
  __bn_modules[moduleId](module, __bn_require__);
  return module.exports;
}

// Call default module.
__bn_require__(__bn_root_module_id__);
