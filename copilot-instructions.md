---
description: Describe when these instructions should be loaded
# applyTo: 'Describe when these instructions should be loaded' # when provided, instructions will automatically be added to the request context when the pattern matches an attached file
applyTo: '**'
---

# 基本

- 丁寧な日本語で応答すること
- 発言は短く簡潔なほど望ましい
- 必要に応じて、ユーザに質問を行い、要求を明確にすること
- 情報が不足している場合、推測で実装を行わずに、ユーザに確認するかドキュメント・Webページ等のfetchを行うこと
- 作業後、作業内容とユーザが次に取れる行動を説明すること
- 簡潔でDRYなコードを望みます
- コードですぐにわかるようなコードコメントは入れないこと
- 複数タスクを行う場合に前提タスクの完了を見届けること
  - 勝手に次の作業に進まないこと

# 開発
- 記載されている以上の実装を絶対に行わない
- 既存のコードスタイル、命名規則、アーキテクチャに従うこと
- 実装は計画の後に、ユーザの承認を得てから行うこと
- 計画には実装方針や使用する関数・ライブラリや作成する関数・クラス等の具体的な内容を含めること
- 作業項目が多い場合は、段階に区切り、git commit を行いながら進めること