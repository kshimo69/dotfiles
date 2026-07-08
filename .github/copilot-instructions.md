---
applyTo: '**'
---

# 基本

- 丁寧な日本語で応答すること
- 発言は短く簡潔なほど望ましい
- 必要に応じて、ユーザに質問を行い、要求を明確にすること
- 情報が不足している場合、推測で実装を行わずに、ユーザに確認するかドキュメント・Webページ等のfetchを行うこと
- 作業後、計画と作業内容とユーザが次に取れる行動を説明すること
- 簡潔でDRYなコードを望みます
- コードですぐにわかるようなコードコメントは入れないこと
- 複数タスクを行う場合に前提タスクの完了を見届けること
  - 勝手に次の作業に進まないこと

# 開発

- 記載されている以上の実装を絶対に行わない
- 既存のコードスタイル、命名規則、アーキテクチャに従うこと
- 実装前に計画を立てること
- 計画には実装方針や使用する関数・ライブラリや作成する関数・クラス等の具体的な内容を含めること
- 作業項目が多い場合は、段階に区切り、git commit を行いながら進めること

# リポジトリ概要

Ansible で管理するパーソナル dotfiles。Mac / RHEL / Ubuntu に対応。

## セットアップコマンド

```bash
# 初回インストール（OS自動検出）
curl -LSfs https://raw.githubusercontent.com/kshimo69/dotfiles/master/install.sh | bash

# 全ロール適用（更新）
ansible-playbook --force-handlers ./playbooks/local.yml

# 特定ロールのみ実行
ansible-playbook --force-handlers ./playbooks/local.yml --tags "base"
ansible-playbook --force-handlers ./playbooks/local.yml --tags "common"
ansible-playbook --force-handlers ./playbooks/local.yml --tags "envs"
```

## アーキテクチャ

### Ansible ロール構成

`playbooks/local.yml` が 3 ロールを順に適用する：

| ロール | 役割 |
|--------|------|
| `base` | OS パッケージマネージャによる基本パッケージインストール |
| `common` | Homebrew パッケージ・Cask アプリのインストール、macOS 設定 |
| `envs` | dotfiles のシンボリックリンク作成、言語バージョン管理ツールのセットアップ |

### OS 別タスクの切り替え

各ロールの `tasks/main.yml` は `ansible_facts['distribution']` を使い、以下の優先順位でタスクファイルを選択する：

```
{distribution}-{major_version}.yml → {distribution}.yml → {os_family}.yml
```

例: `MacOSX.yml`, `Ubuntu.yml`, `CentOS.yml`

### シンボリックリンクの仕組み

`envs` ロールが `~/.dotfiles/` 内のファイルを `~/` 以下にリンクする：

- トップレベルのファイル（`zshrc`, `tmux.conf` 等）→ `~/.<name>`
- `config/` 配下のディレクトリ/ファイル → `~/.config/<name>`

新しい設定ファイルを追加するときは `playbooks/roles/envs/tasks/main.yml` の "Lynk config files" タスクにエントリを追加する。

## 主要な規約

- **Homebrew はすべての OS で使用**：Linux では `/home/linuxbrew/.linuxbrew/bin/brew`
- **冪等性の保証**：一度だけ実行したいタスクには `args: creates: <path>` ガードを付ける
- **パッケージ定義は `defaults/main.yml`**：`homebrew_packages` / `homebrew_cask_packages` で一元管理
- **言語バージョン管理**：`mise`（新）と `anyenv`（旧）を併用。設定は `config/mise.toml`
- **ローカル秘匿設定**：`~/.gitconfig.local`、`~/.netrc`、`~/.passwd` はリポジトリ管理外
