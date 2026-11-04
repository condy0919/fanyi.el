# fanyi.el 改进建议清单

核对基准：截至 `d8dc916` 的实现。`[x]` 表示已完成；`[ ]` 表示尚未实施，具体区分为“暂缓”或“未采纳”，并在对应项中记录原因。下列“实施前”和原建议保留作为背景，不代表当前代码仍存在这些问题。

## 1. 代码实现与性能优化

- [x] **缩小 `fanyi-shr--post-command` 的触发范围**（`fanyi-shr.el`）
  - **已完成**（`ca69349`）：改用 buffer-local 的 `post-command-hook` / `text-scale-mode-hook`，保留窗口变化与主题启停刷新。普通 buffer 的命令不再扫描词典样式；在其他 buffer 任意修改 face 或选项后，返回结果窗口或执行 `fanyi-shr-reflow` 时刷新。
  - **实施前**：`fanyi-shr-insert` 将 `fanyi-shr--post-command` 挂载到了**全局** `post-command-hook`。只要 `*fanyi*` 窗口可见（例如分屏查词），用户在任意 buffer 的每次按键都会遍历 `fanyi-shr--buffers` 并调用 `fanyi-shr--layout` 及各词典的 `style-function`（如 `fanyi-longman--render-style`、`fanyi-haici--render-style`），每次执行 20+ 次 `face-attribute`、`window-font-width`、`image-type-available-p` 与 `copy-tree`。
  - **原建议**：窗口尺寸和焦点切换已由 `window-size-change-functions` 与 `window-selection-change-functions` 处理；其余样式刷新可改为依赖 `*fanyi*` **buffer-local** 的 `post-command-hook` / `text-scale-mode-hook` 以及 Emacs 29+ 的 `enable-theme-functions` / `disable-theme-functions`。

- [x] **优化重排（Reflow）时锚点定位的扫描效率**（`fanyi-shr.el`）
  - **已完成**（`b3208b3`）：按连续空白／非空白文本块扫描，并提取可复用的偏移统计与定位函数，保留原有锚点语义。
  - **实施前**：`fanyi-shr--anchor` 与 `fanyi-shr--position` 通过逐字符循环调用 `(re-search-forward "[^ \t\r\n]" ...)` 统计非空白字符偏移。在 Longman 长词条（数万字符）且存在多个折叠 overlay、选区、窗口时，单次重排会执行数万乃至十万次正则搜索。
  - **原建议**：使用 `skip-chars-forward " \t\r\n"` 配合 `skip-chars-forward "^ \t\r\n"` 按连续非空白文本块计算长度，显著减少循环次数和正则开销。

- [x] **修正 `fanyi-haici--render-style` 的样式签名**（`fanyi-haici.el`）
  - **已完成**（`45acaaa`）：移除仅影响初始折叠的配置，补齐 SVG/XPM 可用性检测。
  - **实施前**：`fanyi-haici--render-style` 包含了 `fanyi-haici-expanded-sections`，但 `fanyi-shr--replace` 在重排时会保留并恢复当前 buffer 已有的折叠状态（文档亦说明该配置仅对新查询生效），导致修改该变量会触发一次无视觉变化的冗余重绘；此外相比 `fanyi-longman--render-style`，海词缺少对 `(image-type-available-p 'svg)` 和 `(image-type-available-p 'xpm)` 的检测。
  - **原建议**：从 `fanyi-haici--render-style` 中移除 `fanyi-haici-expanded-sections`，并补齐图片类型可用性检测。

- [x] **规范化跨行查询词的内部空白字符**（`fanyi.el`）
  - **已完成**（`da5fee2`）：空格、制表符和换行统一折叠为单个空格；显示与历史保留规范化后的大小写，请求继续使用小写。
  - **实施前**：`fanyi-dwim` 仅对输入做了 `string-trim`。当用户在正文中跨折行选中短语（包含 `\n`、`\t` 或连续空格）调用 `fanyi-dwim2` 时，请求 URL 会包含 `%0A` 或 `%20%20`，导致短语查词失败。
  - **原建议**：将查询词内部的连续空白字符折叠为单个空格（如 `(replace-regexp-in-string "[ \t\r\n]+" " " ...)`）。

---

## 2. 错误处理与词典语义区分

- [x] **区分“词典未收录（`not-found`）”与“响应解析失败（`fanyi-parse-error`）”**（`fanyi.el`、`fanyi-haici.el`、`fanyi-longman.el`）
  - **已完成核心区分**（`19d7e9a`）：明确的未收录响应显示 `No results found`，计入完成数，不显示重试按钮。海词要求明确的未收录标题和正文提示；Longman 要求可见的 `didyoumean` 列表及有效词条链接。未知页面仍报解析错误，避免把页面结构变化或拦截页当作正常空结果。
  - [ ] **可选：Longman 拼写建议候选词按钮（暂缓）**。目前缺少真实未收录页面 HTML 样本，现有依据是保存的 CSS 与合成测试。暂不据此扩展候选数据协议和 UI；取得真实样本后再验证候选文本、链接和交互。
  - **实施前**：
    - `fanyi-haici-parse` 与 `fanyi-longman-parse` 在页面不含词条释义时一律抛出 `fanyi-parse-error`（显示 `Could not parse dictionary response`）。例如查询中文词或生僻词时，朗文只是未收录该词，却报错并渲染 `Retry dictionary` 按钮。
    - 即使像 `fanyi-youdao-thesaurus-parse` 那样通过 `(throw 'not-found nil)` 报告无结果，`fanyi--handle-response` 也会将其转为 `(error "No results found")`，计入红色的 `fanyi--tasks-failed` 并渲染 `Retry dictionary` 按钮（对未收录词重试并无意义）。
  - **原建议**：
    - 在海词和朗文解析器中识别正常的“未找到词条/拼写建议”页面并 `(throw 'not-found nil)`（若朗文返回了拼写建议列表，还可渲染为可点击的候选词按钮）。
    - 在任务结算逻辑中将 `not-found` 与网络/解析错误区分开，展示温和的“未收录/无结果”提示，不计入失败数且不显示重试按钮。

- [x] **为 `fanyi-provider` 与 `fanyi-request` 增加 `:name` 字段**（`fanyi-base.el`、`fanyi.el`）
  - **已完成**（`0d21d3e`）：名称保存到请求快照，统一成功标题、失败标题和日志；未设置名称的自定义 provider 回退到 URL host。
  - **实施前**：成功渲染时词典标题为 `# 海词`、`# Longman`、`# 有道同义词词典`，但请求失败或超时时，`fanyi--write-task-error` 只能从 URL 中提取 host，显示为 `# dict.cn`、`# www.ldoceonline.com`、`# dict.youdao.com`，导致前后标题和 Imenu 索引不一致。
  - **原建议**：在 `fanyi-provider` 和 `fanyi-request` 结构体中增加 `:name` 字段，统一正常渲染、错误提示与日志中的词典显示名称。

---

## 3. 交互体验与功能完善

- [x] **支持 Buffer 内的后退/前进历史栈（Back / Forward）**（`fanyi.el`）
  - **已完成**（`4cf48a7`）：`l/r` 调用 `fanyi-back` / `fanyi-forward`，恢复已解析结果、阅读位置、选区、窄化和折叠状态；仅重启未完成请求。`fanyi-navigation-history-length` 默认限制前后栈合计 20 页，新查询清空前进栈，`g` 刷新不增加重复历史项。
  - [ ] **可选：独立的查询结果 LRU 缓存（未采纳）**。已解析结果随有界历史页缓存，已满足后退／前进无需重复请求的需求。暂不再维护一套按查询词索引的 LRU，以免增加缓存键、配置变化及失效策略的复杂度；普通查询和 `h` 仍发起新请求。
  - **实施前**：点击同义词、搭配、词族等交叉引用按钮会在当前 `*fanyi*` buffer 发起新查询并覆盖原内容，但缺少像 `help-mode` / `Info-mode` / `eww` 那样的后退/前进命令（如 `l` / `r` 或 `b` / `f`），只能按 `h`（`fanyi-from-history`）重新选择并重新请求网络。
  - **原建议**：维护 buffer-local 的浏览历史栈（支持后退/前进并恢复阅读位置），并可配合最近 N 次查询的已解析结果内存缓存（LRU），实现秒级后退。

- [x] **丰富 `fanyi-mode-map` 的键盘导航与操作快捷键**（`fanyi.el`）
  - **已完成**（`9157c22`）：`n/p` 跳转可见标题，`v/V` 优先播放当前词典的英式／美式词头发音，`g` 刷新当前查询且支持改名后的结果 buffer。
  - **折叠方案调整**：用 `C-c TAB` 统一切换大纲标题和局部折叠节，保留原来的 `TAB` 按钮导航，避免改变既有操作习惯；另提供 `C-c C-n/C-p`，在标题上也能继续跳转按钮。
  - **章节快速跳转**：长词条往往有上百个按钮，单靠 `TAB` 难以跨词典浏览。建议增加在大纲标题（`#` / `##`）或局部折叠节之间跳转的快捷键（如 `n` / `p` 或 `C-c C-n` / `C-c C-p`）。
  - **一键发音**：增加快捷键（如 `v` / `V` 或 `a`）直接播放当前词条的默认（英/美）发音，无需先将光标移到 `🔊` 按钮上。
  - **支持 `g` (`revert-buffer`)**：设置 `revert-buffer-function` 为重新查询当前词（或重试失败的词典），符合 `special-mode` 派生模式的惯例。
  - **统一折叠交互**：目前 `#` / `##` 标题用 `TAB` 折叠（`fanyi-mode-cycle-map`），而海词/朗文的局部折叠节（`fanyi-shr-section-button`）按 `TAB` 会跳到下一个按钮、必须按 `RET` 折叠，可提供更统一的折叠按键体验。

- [x] **优化 Header Line 状态展示**（`fanyi.el`）
  - **已完成**（`51a3f56`）：无待完成请求时显示 `Ready`，计数增加 `Pending / Done / Failed` 标签及 `help-echo` 提示。
  - **实施前**：所有词典加载完毕后，`fanyi-format-header-line` 仍一直显示 `"Translating <word> 0 4 0"`，且三个裸数字无说明。
  - **原建议**：当 pending 数为 0 时更新状态文案，并为三个计数器添加 `help-echo` 提示（如 `"Pending / Completed / Failed dictionaries"`）。

---

## 4. 配置细节与工程化

- [x] **完善音频播放器自定义选项**（`fanyi-base.el`）
  - **已完成**（`0b5f0b5`）：播放器 Customize 类型支持 `nil`，增加 ffplay 检测和必要播放参数，新增公开的 `fanyi-sound-player-options`。
  - **接口方案调整**：保留私有函数 `fanyi--sound-player-options` 生成必要默认参数，而不是直接将它改成变量。公开选项提供附加参数，既允许自定义，也保留 mplayer／ffplay 默认行为；每次播放保存独立参数快照。
  - `fanyi-sound-player` 在未检测到播放器时默认值为 `nil`，但其 `:type` 为 `'string`（Customize 界面会报类型不匹配），应改为 `'(choice (const :tag "None" nil) file string)`；同时可补充检测 `ffplay` 等常见工具。
  - 将 `fanyi--sound-player-options` 暴露为 `defcustom`，方便用户自定义播放器参数（如音量控制等）。

- [ ] **统一外部浏览器调用函数**（`fanyi-etymon.el`）
  - **未采纳，保留现有实现**：Etymonline 已明确定位为外部浏览器入口，用于访问受 Cloudflare 保护的页面。改用 `browse-url` 可能遵循用户的 EWW 配置重新在 Emacs 内打开，回到原来的访问限制，因此继续使用 `browse-url-default-browser`。这是有意保留的行为差异，不是遗漏。
  - `fanyi-etymon-render` 直接调用了 `browse-url-default-browser`，绕过了用户在 Emacs 中配置的 `browse-url-browser-function` 与 `browse-url-handlers`；建议与 `fanyi-longman--shr-link` 保持一致，改用 `browse-url`。

- [x] **改进 `ol-fanyi.el` 的模式判断与自动加载**（`ol-fanyi.el`）
  - **已完成**（`1a77c4f`）：使用 `derived-mode-p` 支持派生模式，通过包 autoload 在 Org 加载后注册链接；直接加载源码且未生成包 autoload 时，仍可手动 `(require 'ol-fanyi)`。
  - `fanyi-org-store-link` 中使用 `(derived-mode-p 'fanyi-mode)` 替代 `(eq major-mode 'fanyi-mode)`。
  - 利用 `;;;###autoload` 配合 `(with-eval-after-load 'ol ...)` 自动注册 `fanyi:` 链接类型，免去用户手动 `(require 'ol-fanyi)`。

- [x] **更新 CI 工作流、Makefile 与清理残留文件**（`.github/workflows/build.yml`、`Makefile`、`test/`）
  - **已完成**（`310feb6`）：自动发现 `test/*-tests.el`，清理下列三份备份文件，CI 矩阵为 Emacs 29.1、30.2 和 snapshot。
  - **版本方案调整**：checkout 实际升级为 v7，而非建议中的 v4；30.x 使用 30.2，没有另加 30.1，以避免同一大版本重复执行整套检查的成本，同时保留最低支持版本与开发版本覆盖。
  - 将 `.github/workflows/build.yml` 中的 `actions/checkout@v2` 升级为 `actions/checkout@v4`，并在测试矩阵中补充 Emacs `30.1` / `30.2`。
  - `Makefile` 的 `test` 目标目前手工罗列了 18 个测试文件，可改为自动发现 `test/*-tests.el`。
  - 清理 `test/` 目录下残留的编辑器备份文件（`test/fanyi-shr-tests.el~`、`test/fanyi-longman-section-tests.el~`、`test/fanyi-shr-section-tests.el~`）。
