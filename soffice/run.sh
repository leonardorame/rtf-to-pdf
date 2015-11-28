#!/bin/bash
echo "Atención: este programa se ejecuta desde cron."

pidof /usr/lib/libreoffice/program/soffice.bin  >/dev/null
if [[ $? -ne 0 ]] ; then
        echo "Restarting Soffice:     $(date)" >> /home/martin/soffice/restart.log
        soffice --invisible --accept="socket,host=127.0.0.1,port=2002,tcpNoDelay=1;urp;" --headless --nodefault --nofirststartwizard --nolockcheck --nologo --norestore &
fi
