# vst24pas

vst2.4 for pascal

Pascal 语言实现的 VST 2.4

你可以用它创建一个同时支持 win32, win64 甚至是原生 linux 的 vst2 插件,
但是不推荐在 linux 下使用 lcl, 因为 lcl 并不稳定, 所以最好不要开发 GUI 插件

由于本人水平有限, 可能存在 bug

## 安装和使用

支持 Free Pascal 和 Delphi

具体请看例子

PS:

- FPC 3.2.2 和 3.3.1 测试 OK

- Delphi 10.3.3 社区版测试 OK

### lazarus

打开 vst24pas.lpk, 选择 使用 - 添加到工程

以后只需要 新建工程-库, 打开工程查看器, 选择 添加 - 新建需要, 搜索 vst24pas, 找到后选择并确定

### delphi

添加 source 目录到 search path
