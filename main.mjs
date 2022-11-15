import main from "./output.mjs";

const pair = (_) => (_) => (a) => (b) => [a, b];

const fst =
  (_) =>
  (_) =>
  ([a, _]) =>
    a;

const snd =
  (_) =>
  (_) =>
  ([_, b]) =>
    b;

console.log(main(null)(pair)(fst)(snd)(null)(null)(["A", 10]));
