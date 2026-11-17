// spawn/await：隐式全移动捕获 + 任务隔离 + 结果经 Task 交付。
// FIFO 确定性：main continues 先打印，子任务在其后运行。
// expect: main continues
// expect: 21
// expect: 42
let n = 21;
let t = spawn(do () { print(n); return n * 2; });
print("main continues");
let v = t.await;
print(v);
