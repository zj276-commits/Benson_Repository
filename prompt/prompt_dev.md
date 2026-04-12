# Market Insight Studio — 开发提示词 v3

## 1. 这次要解决的核心问题

当前项目已经具备这些基础能力：
- R/Shiny 金融分析 Dashboard
- 7 只美股科技股的数据拉取与量化指标
- RAG + 多 Agent 报告生成
- mask-safe 回测框架
- RAG 质量矩阵与确定性测试

但交易部分仍然存在一个关键缺陷：

**系统更像“给一次建议”，不像“持续交易的 Agent”。**

表现为：
- 在一个时间窗口里往往只发生 1 次交易
- 策略逻辑单一，几乎是单一阈值驱动
- prompt 没有明确要求 active trading
- 动作空间过粗，只有 `BUY / SELL / HOLD`
- 每日输入信号变化不够大，导致模型容易一直 `HOLD`

这次迭代的目标不是继续堆工具，而是先把交易 Agent 做成一个真正有“多策略决策能力”的系统。

---

## 2. 产品目标重定义

### 目标版本
从：
- 单次建议式交易助手

升级为：
- **多策略主动交易 Agent**
- **按天滚动决策**
- **严格 mask 机制**
- **能解释为什么今天选择这个策略**
- **可以和 buy-and-hold 做对照**

### 对用户的可见价值
用户打开页面后，不只是看到“建议买/卖”，而是能看到：
- 在自定义时间窗口里，Agent 每天怎么决策
- 当天主要采用了哪类策略
- 决策依据来自哪些价格信号、新闻信号、RAG 上下文
- 最终收益曲线相对 buy-and-hold 是否更优

---

## 3. 为什么 v1 容易只有 1 次交易

根因不是模型“笨”，而是系统设计天然让它不活跃：

1. **动作空间太粗**
   - 只有 `BUY / SELL / HOLD`
   - 模型一旦买入，后续很容易因为“没有极强反向信号”而一直不动

2. **输入信号过于静态**
   - 主要依赖年化 GBM drift、基础新闻、长期指标
   - 这些信号在 7 天到 30 天窗口内变化不够快

3. **没有策略层**
   - 系统没有显式告诉模型“你可以做趋势、均值回归、事件驱动、防守减仓”等不同玩法
   - 模型只能在一个模糊风格里做决策

4. **没有仓位语言**
   - 没有 `target_position_pct`
   - 不能部分加仓、减仓、试探性建仓

5. **没有行为约束目标**
   - 没有要求“在有信号时积极交易”
   - 没有最小交易活跃度、策略多样性、信号一致性等 eval

结论：
**不是先去接更多服务，而是先重做交易决策协议。**

---

## 4. 本轮开发原则

### 原则 1：先解决核心链路，再扩服务
优先完成：
- 多策略交易决策
- mask-safe 输入
- 决策解释
- 回测评估

暂不把大量精力放在：
- TradingAgents 微服务
- Braintrust 生产监控
- Alpaca 真正执行
- 全量 ChromaDB 重构

这些可以后置。

### 原则 2：最小依赖可落地
优先使用当前 repo 已有能力：
- `global.R`
- `models.R`
- `rag.R`
- `backtester.R`
- `server.R`
- `ui.R`

只在“明显增益 > 集成成本”时再新增依赖。

### 原则 3：策略层和风控层分开
交易 Agent 不应直接裸输出买卖，而应先经过：
- 策略评分层
- 风险约束层
- 仓位执行层

### 原则 4：评估先行
任何新策略都必须能被 matrix 检验：
- 是否有 look-ahead
- 是否真的提升交易活跃度
- 是否策略多样
- 是否比 baseline 更合理

---

## 5. 推荐的系统目标形态

### 交易系统从单模型建议，升级为四层结构

```
Layer 1: Masked Feature Engine
  - 只基于 [t-lookback, t] 的价格/新闻/模型参数
  - 输出每日短期交易特征

Layer 2: Strategy Router
  - 将当天状态映射为多个候选策略分数
  - 例如趋势、均值回归、事件驱动、防守模式

Layer 3: LLM Decision Agent
  - 读取结构化特征 + RAG 新闻上下文 + 最近几天决策记忆
  - 输出目标仓位和策略解释

Layer 4: Risk Overlay + Execution
  - 仓位限制、交易成本、不得超买/超卖
  - 生成真实交易记录和收益曲线
```

这才是“Agent 在交易”，而不是“LLM 只回答一句建议”。

---

## 6. 多策略交易 Agent 设计

### 6.1 必须引入的策略库

至少支持 4 类策略：

| 策略名 | 适用场景 | 关键输入 | 典型动作 |
|------|------|------|------|
| `trend_following` | 上升趋势明确 | SMA5/SMA10, 连涨天数, 量价配合 | 逐步加仓 |
| `mean_reversion` | 超跌反弹 | RSI, 短期跌幅, 异常放量 | 低位试探买入 |
| `event_momentum` | 新闻冲击明显 | 新闻情感变化, 事件方向, 相关性分数 | 事件后跟随 |
| `defensive_risk_off` | 波动和负面事件上升 | 波动率扩张, drawdown, 负面新闻 | 减仓或空仓观望 |

可选第 5 类：

| 策略名 | 适用场景 | 关键输入 | 典型动作 |
|------|------|------|------|
| `range_rebalance` | 无趋势震荡 | 波动收敛, 横盘区间, 无强新闻 | 小幅调仓 |

### 6.2 不再只输出 BUY/SELL/HOLD

新的决策协议应该改成：

```json
{
  "selected_strategy": "trend_following",
  "target_position_pct": 65,
  "confidence": 78,
  "rebalance_reason": "Short-term trend remains positive while risk is moderate.",
  "signal_scores": {
    "trend_following": 0.81,
    "mean_reversion": 0.24,
    "event_momentum": 0.57,
    "defensive_risk_off": 0.19
  },
  "risk_flags": [
    "volatility_elevated",
    "news_flow_mixed"
  ],
  "holding_horizon": "2-5 trading days"
}
```

然后由执行层把 `target_position_pct` 转成实际交易股数。

### 6.3 决策记忆

给交易 Agent 的输入中增加最近 3 天：
- 上次目标仓位
- 上次选择的策略
- 上次推理摘要
- 上次是否错过趋势或过度交易

这样能避免：
- 今天买 100%，明天无理由又卖光
- 连续几天重复说同样的话

---

## 7. 交易输入信号必须升级

### 7.1 每日短期价格信号

必须新增而不是只靠年化模型：
- 1D / 3D / 5D / 10D 收益率
- RSI(5), RSI(14)
- SMA5 / SMA10 / SMA20
- 价格相对 SMA 偏离
- 20 日年化波动率
- 今日波动率相对过去 5 日变化
- 成交量相对 20 日均量倍率
- 连涨/连跌天数
- 最近回撤

### 7.2 新闻驱动信号

优先做轻量级版本：
- 今日相关新闻数
- 今日净情感分
- 今日 vs 昨日情感变化
- 是否存在高相关事件
- 事件方向：利多 / 利空 / 中性

### 7.3 RAG 输入不要只是“给新闻原文”

要把 RAG 输出拆成两部分：

1. **给 LLM 的文本上下文**
   - 精选新闻摘要
   - 关键财务/风险段落

2. **给策略路由器的结构化特征**
   - `news_sentiment_today`
   - `news_sentiment_change_1d`
   - `high_impact_event_count`
   - `macro_risk_score`

这样模型才会真的“按信号交易”，而不是只看长文本。

---

## 8. RAG 的定位要调整

### 当前建议
现在不要把重点放在“大规模迁移到 ChromaDB”。

原因：
- 当前 repo 已经有可用的 `rag.R`
- 新闻检索和质量 matrix 已经能跑
- 交易层真正缺的是策略设计，不是向量库

### 更合理的顺序

#### Phase A：继续使用当前 RAG
- 保留现有 CSV + 语义检索增强
- 继续用当前已验证的 RAG matrix
- 强化交易用到的新闻特征提取

#### Phase B：如果 key 和时间允许，再接外部增强
- `MARKETAUX_API_TOKEN`：补充新闻与情感
- `ALPHA_VANTAGE_API_KEY`：补充 news sentiment
- `TIINGO_API_TOKEN`：补充更稳定的历史行情

#### Phase C：最后再考虑 ChromaDB
- 当新闻量显著增加时再上
- 主要收益在新闻语义检索，不在结构化财务表

结论：
**RAG 先服务交易，不要让 RAG 重构反过来阻塞交易系统。**

---

## 9. UI 目标

这轮前端重点不是花哨，而是信息结构清楚。

### 输入区
- ticker
- custom range
- model 选择
- strategy mode：
  - `Auto (Agent chooses)`
  - `Trend only`
  - `Mean reversion only`
  - `Event momentum only`
  - `Defensive only`

### 输出区

#### 主输出
- 收益曲线：Agent vs Buy-and-Hold
- 交易记录表
- 当前回测 summary card

#### 新增重点面板
- **Why this decision?**
  - 今日选用策略
  - 关键价格信号
  - 关键新闻事件
  - 风险限制

#### 辅助输出
- 小型质量卡片
  - look-ahead: pass/fail
  - JSON schema: pass/fail
  - RAG retrieval: pass/fail
  - trade constraints: pass/fail

不再让 Quality Matrix 占据大块页面面积。

---

## 10. 评估体系要围绕“多策略主动交易”重写

### 10.1 硬约束指标

这些是必须 100% 过线的：

| 指标 | 类型 | 目标 |
|------|------|------|
| No Look-Ahead Bias | 确定性 | 100% |
| JSON Schema Validity | 确定性 | > 99% |
| No Negative Cash | 确定性 | 100% |
| No Negative Shares | 确定性 | 100% |
| Equity Curve Length | 确定性 | 正确 |

### 10.2 行为质量指标

这些用来判断系统是否还在“只交易一次”：

| 指标 | 含义 | 目标 |
|------|------|------|
| Trade Count | 窗口内交易次数 | 明显大于 1 |
| Active Decision Ratio | 非 HOLD / 有仓位调整的比例 | 不应接近 0 |
| Strategy Diversity | 实际触发的策略种类数 | 30 日窗口至少 2 类 |
| Rebalance Smoothness | 仓位调整是否过于极端 | 避免每天满仓/空仓切换 |
| Hold Streak | 连续 HOLD 天数 | 不应长期僵死 |

### 10.3 结果指标

| 指标 | 目标 |
|------|------|
| Alpha vs Buy-and-Hold | 尽量为正，但不设为硬门槛 |
| Sharpe Ratio | 尽量提升 |
| Max Drawdown | 控制不劣化太多 |
| Win Rate | 作为参考，不单独决定好坏 |

### 10.4 RAG 指标

这一部分已经有基础，不用推倒重来：

| 指标 | 目标 |
|------|------|
| Retrieval Precision | 高 |
| Freshness | 达标 |
| Ticker Contamination | 0 |
| Context Budget | 不超标 |
| Company-news Precision | 尽量接近 1 |

### 10.5 推荐的达标标准

可以先采用这组现实阈值：

- `lookahead_pass_rate = 100%`
- `schema_pass_rate >= 99%`
- `trade_constraint_pass_rate = 100%`
- `trade_count_median >= 3`（在 30 交易日窗口）
- `strategy_diversity_median >= 2`
- `rag_pass_rate >= 95%`

这里最重要的是：
**把“交易次数 > 1”从偶然结果变成一个被测试覆盖的目标。**

---

## 11. 推荐的开发顺序

### Phase 1：先把多策略交易决策做出来

只依赖现有 key：
- `OPENAI_API_KEY`
- `FINNHUB_API_KEY`

任务：
1. 给 `backtester.R` 增加 `target_position_pct`
2. 新增 `compute_short_signals()`
3. 新增 `compute_strategy_scores()`
4. 改造 trading prompt，明确要求 active trading
5. 新增 `strategy_mode`
6. 输出 Buy-and-Hold baseline

### Phase 2：增强新闻信号

优先推荐只拿一个外部 key：
- **首选：`MARKETAUX_API_TOKEN`**

原因：
- 直接补新闻与情感
- 接入成本低
- 对交易层帮助最大

然后再考虑：
- `ALPHA_VANTAGE_API_KEY`
- `TIINGO_API_TOKEN`

### Phase 3：做评估闭环

任务：
1. 设计多窗口回测集合
2. 对 7 个 ticker 跑批量 backtest
3. 汇总交易次数、策略多样性、alpha、drawdown
4. 找出“又退回单次交易”的失败 case
5. 继续迭代 prompt 和策略打分

### Phase 4：最后再考虑重服务化

如果前 3 阶段稳定，再考虑：
- ChromaDB
- Alpaca paper trading
- FinBERT sidecar
- Braintrust

---

## 12. API Key 优先级

### 现在就够用
- `OPENAI_API_KEY`
- `FINNHUB_API_KEY`

### 你现在最值得去拿的
- **`MARKETAUX_API_TOKEN`**

### 第二优先级
- `TIINGO_API_TOKEN`
- `ALPHA_VANTAGE_API_KEY`

### 暂时不急
- `ALPACA_API_KEY`
- `BRAINTRUST_API_KEY`

### 不需要 key
- ChromaDB 本地 / Docker

如果你这轮只能先拿一个新 key，就先拿：

**`MARKETAUX_API_TOKEN`**

因为它对“新闻驱动交易策略”帮助最大，且接入快。

---

## 13. 建议的代码入口

本轮优先改这些文件：

| 文件 | 这轮职责 |
|------|------|
| `backtester.R` | 多策略交易循环、target_position_pct、执行逻辑 |
| `models.R` | 短期价格信号计算 |
| `rag.R` | 交易可用的新闻上下文与结构化新闻特征 |
| `server.R` | strategy_mode、模型选择、回测触发 |
| `ui.R` | 策略模式选择、解释卡片、baseline 对比 |
| `tests/test_suite.R` | 新增 trade activity / strategy diversity 测试 |
| `tests/test_rag_eval.R` | 保持 RAG 质量回归 |

---

## 14. 本文件的最终目标

这份计划的核心不是“列出尽可能多的工具”，而是约束开发优先级：

1. 先修复“只交易一次”
2. 再增强新闻和语义信号
3. 再做规模化 eval
4. 最后才做更重的服务化和外部集成

一句话总结：

**本轮不是去做更大的系统，而是把交易 Agent 从“单策略建议器”升级成“多策略、可解释、可评估的主动交易系统”。**
