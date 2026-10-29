// 不可变引用不允许经解引用写。
// error: cannot assign through an immutable reference
let x = 1;
let r = &x;
*r = 2;
