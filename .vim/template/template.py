#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Author:   Kimihiko Shimomura
# License:  MIT License
# Created:  <+DATE+>
#

from logging import getLogger,StreamHandler,DEBUG
logger = getLogger(__name__)
handler = StreamHandler()
handler.setLevel(DEBUG)
logger.setLevel(DEBUG)
logger.addHandler(handler)

logger.debug('hello')

<+CURSOR+>
