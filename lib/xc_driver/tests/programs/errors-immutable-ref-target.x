// &mut 的目标必须是 mut 绑定。
// error: cannot take a mutable reference to immutable variable
let x = 1;
let r = &mut x;
