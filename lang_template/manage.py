#! /usr/bin/env python

import os

from langkit.libmanage import ManageScript


class Manage(ManageScript):
    def create_context(self, args):
        from langkit.compile_context import CompileCtx

        from language.lexer import template_lexer
        from language.parser import template_grammar

        return CompileCtx(lang_name='Template',
                          lexer=template_lexer,
                          grammar=template_grammar)

if __name__ == '__main__':
    Manage().run()
