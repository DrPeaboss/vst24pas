# vst24pas

vst2.4 for pascal

Pascal 语言实现的 VST 2.4

你可以用它创建一个同时支持 win32, win64 甚至是原生 linux 的 vst2 插件, 但是不推荐在 linux 下使用 lcl, 因为 lcl 并不稳定

由于本人水平有限, 可能存在 bug

## 安装和使用

支持 Free Pascal 和 Delphi

具体请看例子

PS:

- FPC 3.2.0 和 3.3.1 测试 OK

- Delphi 10.3.3 社区版测试 OK

### lazarus

打开 vst24pas.lpk, 编译

新建工程-库, 在需要的包处添加 vst24pas

### delphi

添加 source 目录到 search path
