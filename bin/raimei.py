# -*- coding: utf-8 -*-

try:
    import vim
except ImportError:
    print "call ':pyfile raimei' from vim"

import sys
from ikazuchi import izuchi
from locale import getdefaultlocale


def eval_or_default(param, default):
    try:
        ret = vim.eval(param)
    except:
        # FIXME: cannot catch vim.error
        ret = default
    return ret

def get_vim_variables():
    # settings from VimScript(.vimrc)
    _envvar = getdefaultlocale()
    api_name = eval_or_default("raimei_api", "google")
    lang_from = eval_or_default("raimei_from", "en")
    lang_to = eval_or_default("raimei_to", _envvar[0].split("_")[0])
    encoding = eval_or_default("&enc", _envvar[1])
    return api_name, lang_from, lang_to, encoding

def translate_with_range(translator, encoding):
    apis, translated = set(), []
    vim.current.range.append("")
    for line in vim.current.range:
        for info in translator.translate(unicode(line, "utf-8")):
            apis.add(info[0])
            translated.append(info[1].encode(encoding))
    # append translated text into vim in reverse
    for text in reversed(translated):
        vim.current.range.append(text)
    print "Translated by {0}".format(list(apis))

def translate(api_name, lang_from, lang_to, encoding):
    translator = izuchi.TRANSLATE_API[api_name](lang_from, lang_to, None)
    return translate_with_range(translator, encoding)

def main():
    try:
        vim
    except NameError:
        return
    vim_vars = get_vim_variables()
    translate(*vim_vars)

if __name__ == "__main__":
    main()
