# -*- coding: utf-8 -*-

from ansible.plugins.callback import CallbackBase
import platform
import subprocess


class CallbackModule(CallbackBase):

    def v2_playbook_on_stats(self, stats):
        if platform.system() != 'Darwin':
            return

        hosts = sorted(stats.processed.keys())
        for host in hosts:
            summary = stats.summarize(host)

            status = "Provisioning completed!"
            if summary.get('failures') > 0 or summary.get('unreachable') > 0:
                status = "Provisioning failed!"

            message = ", ".join(["{}: {}".format(k, v) for k, v in summary.items()])
            cmd = "osascript -e 'display notification \"{}\" with title \"{}\"'"
            cmd = cmd.format(message, status)
            subprocess.call(cmd, shell=True)
