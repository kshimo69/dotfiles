---
name: python-coding-standards
description: Pythonコードを書く際の品質とスタイルガイドラインを提供します。特に空行のインデント処理に関する厳格なルールを含みます。
---

# Python コーディング規約スキル

## 概要
Pythonコードを書く際の品質とスタイルガイドラインを提供します。特に空行のインデント処理に関する厳格なルールを含みます。

## 適用条件
- `.py`ファイルを編集する時
- Pythonコードを生成する時
- Jupyter Notebookのコードセルを編集する時

## コーディングルール

### 1. 空行のインデント規則（必須）

**絶対に守るべきルール: 空行には一切のインデント（スペースやタブ）を含めないこと**

❌ **誤った例:**
```python
def example():
    x = 1
····  # 空行にインデントがある（誤り）
    y = 2
```

✅ **正しい例:**
```python
def example():
    x = 1

    y = 2
```

**理由:**
- 多くのリンターやフォーマッターがこれをエラーとして検出
- 不要な空白文字がファイルサイズを増加させる
- コードの可読性を損なわず、ベストプラクティスに準拠

### 2. インデント

- インデントは**スペース4つ**を使用
- タブは使用しない
- 継続行は適切にインデント

```python
# 正しい例
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print(var_one)
```

### 3. 空行の使い方

- トップレベルの関数やクラス定義の前後に**2行の空行**
- クラス内のメソッド定義の間に**1行の空行**
- 関数内で論理的なセクションを分ける場合に**1行の空行**

```python
import os
import sys


class MyClass:
    """クラスの説明"""

    def __init__(self):
        self.value = 0

    def method_one(self):
        """最初のメソッド"""
        return self.value


class AnotherClass:
    """別のクラス"""
    pass


def standalone_function():
    """スタンドアロン関数"""
    pass
```

### 4. 行の長さ

- 1行は最大**79文字**（コメントやdocstringは72文字）
- 長い行は適切に分割

```python
# 長い行の適切な分割
result = some_function_that_takes_arguments(
    'argument one',
    'argument two',
    'argument three'
)
```

### 5. インポート

- インポートは常にファイルの先頭
- 標準ライブラリ、サードパーティ、ローカルの順にグループ化
- 各グループ間に1行の空行

```python
import os
import sys

import numpy as np
import pandas as pd

from myproject import mymodule
```

### 6. 命名規則

- **クラス名**: PascalCase (`MyClass`)
- **関数・変数名**: snake_case (`my_function`, `my_variable`)
- **定数**: UPPER_SNAKE_CASE (`MAX_VALUE`)
- **プライベート**: アンダースコア始まり (`_private_method`)

### 7. 文字列

- シングルクォートとダブルクォートは一貫性を保つ
- 複数行の文字列にはトリプルクォート
- f-string を優先的に使用（Python 3.6+）

```python
# 推奨
message = f"Hello, {name}!"

# 複数行
description = """
これは複数行の
文字列です。
"""
```

### 8. コメント

- コメントは常に最新の状態を保つ
- 複雑なロジックには説明を追加
- docstringで関数・クラス・モジュールを文書化

```python
def calculate_average(numbers):
    """数値のリストの平均を計算する
    
    Args:
        numbers (list): 数値のリスト
    
    Returns:
        float: 平均値
    
    Raises:
        ValueError: リストが空の場合
    """
    if not numbers:
        raise ValueError("リストが空です")

    return sum(numbers) / len(numbers)
```

## チェックリスト

コードを書く際、以下を確認してください：

- [ ] 空行にインデント（空白やタブ）が含まれていないか
- [ ] インデントはスペース4つで統一されているか
- [ ] 行の長さは79文字以内か
- [ ] インポートは適切に整理されているか
- [ ] 命名規則に従っているか
- [ ] 必要な場所にdocstringがあるか
- [ ] 適切な場所に空行が入っているか

## ツールとの統合

以下のツールを使用して自動チェックを推奨：

- **flake8**: コーディングスタイルチェック
- **black**: 自動フォーマット
- **isort**: インポート文の自動整理
- **pylint**: 包括的なコード分析

```bash
# インストール
pip install flake8 black isort pylint

# 実行
flake8 your_file.py
black your_file.py
isort your_file.py
```

## 参考資料

- [PEP 8 -- Style Guide for Python Code](https://peps.python.org/pep-0008/)
- [Google Python Style Guide](https://google.github.io/styleguide/pyguide.html)
