# -*- coding: utf-8 -*-

# Make coding more python3-ish
from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

from ansible import constants as C
from ansible.module_utils.common.text.converters import to_text
from ansible.playbook.task_include import TaskInclude
from ansible.plugins.callback import CallbackBase


class CallbackModule(CallbackBase):
    CALLBACK_VERSION = 2.0
    CALLBACK_TYPE = 'stdout'
    CALLBACK_NAME = 'custom_stdout'
    CALLBACK_NEEDS_WHITELIST = True

    def __init__(self):
        super(CallbackModule, self).__init__()

    def v2_playbook_on_include(self, included_file):
        msg = '  %s included: %s' % (", ".join([h.name for h in included_file._hosts]), included_file._filename)
        if 'item' in included_file._args:
            msg += " => (item=%s)" % (self._get_item_label(included_file._args),)
        self._display.display(msg, color=C.COLOR_SKIP)

    def v2_runner_on_ok(self, result):
        if isinstance(result._task, TaskInclude):
            return
        if result._task.loop and 'results' in result._result:
            self._process_items(result)
        else:
            super(CallbackModule, self).v2_runner_on_ok(result)

    def v2_runner_on_failed(self, result, ignore_errors=False):
        if result._task.loop and 'results' in result._result:
            self._process_items(result)
        else:
            super(CallbackModule, self).v2_runner_on_failed(result)

    def v2_runner_item_on_ok(self, result, msg="ok", display_color=C.COLOR_OK):
        if isinstance(result._task, TaskInclude):
            return
        self._preprocess_result(result)

        result_was_changed = ('changed' in result._result and result._result['changed'])
        if result_was_changed:
            msg = "done"
            display_color = C.COLOR_CHANGED

        msg += ": %s" % (self._get_item_label(result._result),)
        task_result = self._process_result_output(result, msg)
        self._display.display("  " + task_result, display_color)

    def v2_runner_item_on_failed(self, result):
        if isinstance(result._task, TaskInclude):
            return
        self._preprocess_result(result)
        display_color = C.COLOR_ERROR
        msg = "failed"

        msg += ": %s" % (self._get_item_label(result._result),)
        task_result = self._process_result_output(result, msg)
        self._display.display("  " + task_result, display_color)
