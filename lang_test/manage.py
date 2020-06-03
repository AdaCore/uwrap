#! /usr/bin/env python

import os

from langkit.libmanage import ManageScript


class Manage(ManageScript):
    def create_context(self, args):
        from langkit.compile_context import CompileCtx

        from language.lexer import test_lexer
        from language.parser import test_grammar

        return CompileCtx(lang_name='Test',
                          lexer=test_lexer,
                          grammar=test_grammar)

if __name__ == '__main__':
    Manage().run()
