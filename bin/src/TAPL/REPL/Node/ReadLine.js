import rl from "node:readline";


export const getRawInputImpl = (
  handleStroke,
  shouldClose,
) => (() => {
  return new Promise((resolve) => {
    rl.emitKeypressEvents(process.stdin);
    if (process.stdin.isTTY) {
      process.stdin.setRawMode(true);
    }
    const cb = (seq, key) => {
      handleStroke(seq, key);
      if (shouldClose(key)) {
        process.stdin.removeListener("keypress", cb);
        process.stdin.setRawMode(false);
        resolve();
      }
    };
    process.stdin.on("keypress", cb);
  });
})();

export const mkKeyNameImpl = function (KeyCode, KeyString, KeyUndefined, k) {
  if (k === undefined) {
    return KeyUndefined;
  }
  if (typeof k === "string") {
    return KeyString(k);
  }
  if (typeof k === "number") {
    return KeyCode(k);
  }
  throw new Error("Invalid keycode");
}