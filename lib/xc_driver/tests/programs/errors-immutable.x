// 默认不可变：对非 mut 绑定赋值是编译错误（用户裁定 2026-10-28）。
// error: cannot assign to immutable variable
let x = 1;
x = 2;
