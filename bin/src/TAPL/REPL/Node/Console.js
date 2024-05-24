export const write = (msg) => () => {
  process.stdout.write(msg)
}