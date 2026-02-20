# Deploy Key 设置步骤 / Deploy Key Setup

用 Deploy Key 推送到 Benson_Repository 的完整步骤。

---

## 1. 生成专用于此仓库的 SSH 密钥（若还没有）

在终端执行（把邮箱换成你的）：

```bash
ssh-keygen -t ed25519 -C "deploy-benson" -f ~/.ssh/id_ed25519_benson -N ""
```

- 会生成：`~/.ssh/id_ed25519_benson`（私钥）和 `~/.ssh/id_ed25519_benson.pub`（公钥）。

---

## 2. 在 GitHub 上添加 Deploy Key

1. 打开：**https://github.com/zj276-commits/Benson_Repository/settings/keys**
2. 点 **“Add deploy key”**。
3. **Title**：例如 `My laptop` 或 `shiny_app deploy`。
4. **Key**：粘贴公钥内容。在终端执行：
   ```bash
   cat ~/.ssh/id_ed25519_benson.pub
   ```
   复制整行（以 `ssh-ed25519` 开头）。
5. 勾选 **“Allow write access”**（才能 push）。
6. 点 **“Add key”**。

---

## 3. 配置 SSH 使用该密钥访问 Benson_Repository

在 `~/.ssh/config` 里加入（没有这个文件就新建一个）：

```
Host github-benson
  HostName github.com
  User git
  IdentityFile ~/.ssh/id_ed25519_benson
  IdentitiesOnly yes
```

---

## 4. 把本仓库的 remote 改成 SSH（使用上面的 Host）

在 `02_productivity/shiny_app` 目录下执行：

```bash
cd /Users/jinzheyu/dsai/02_productivity/shiny_app
git remote set-url origin git@github-benson:zj276-commits/Benson_Repository.git
git push -u origin main
```

若默认分支是 `master`，最后一句改为：

```bash
git push -u origin master
```

---

完成后，以后在此目录下直接 `git push` 就会用 Deploy Key，无需再输入密码。
