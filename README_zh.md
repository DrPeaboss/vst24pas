# vst24pas

vst2.4 for pascal

Pascal 语言实现的 VST 2.4

你可以用它创建一个同时支持 win32, win64 甚至是原生 linux 的 vst2 插件

但是不推荐在 linux 下使用 lcl, 因为 lcl 并不稳定, 所以最好不要开发 GUI 插件

由于本人水平有限, 可能存在 bug

## 安装和使用

支持 Free Pascal 和 Delphi

具体请看例子

PS:

- FPC 3.2.2 和 3.3.1 测试 OK

- Delphi 10.3.3 和 10.4.2 社区版测试 OK

### lazarus

打开 vst24pas.lpk, 选择 `使用 - 添加到工程`

以后只需要 `新建工程-库`, 打开`工程查看器`, 选择 `添加 - 新建需要`, 搜索 `vst24pas`, 找到后选择并确定

### delphi

在主菜单点击 `Project - Options`, 在工程选项里点击 `Building - Delphi Compiler` 项，在右边的 `Search path` 里添加你电脑里的 `vst24pas\source` 目录

注意上面的 `Target` 选项

## 不再支持 VST 2

详见 https://forums.steinberg.net/t/vst-2-discontinued/761383 (2022/01/19 发布)

未来更多的 DAW 会取消对 VST 2 的支持

是时候应该升级到 VST 3 了

## 关于 VST 3

你可以在这里找到 VST 3 的API <https://github.com/DrPeaboss/vst3-pas>
