# Se utilizara una plantilla de libreoffice, la cual debera tener si o si activado dentro de las propiedades del header autofit en true.
# Format -> Page -> Header -> AutoFit = checked

import io
#import StringIO

import os
import sys

import string

import json
import base64
import subprocess

# Se obtiene el documento por stdin.
input = ""
input_pie = ""
input_cuerpo = ""
input_cabecera = ""
comenzo_cuerpo=0

# Se compone el stdin en un unico texto 
input = ""
for line in sys.stdin:
  input = input + line

# El documento viene en un JSON conteniendo header, body & footer en base64
input = json.loads(input)
input_cabecera = base64.b64decode(input['header']).decode('UTF-8')
input_cuerpo = base64.b64decode(input['body']).decode('UTF-8')
input_pie = base64.b64decode(input['footer']).decode('UTF-8')

# Replace font
def replaceFont(aString):
    p = subprocess.Popen(['./replacefont', 'Lucida Console', '21', '400'], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
    p.stdin.write(bytes(aString, 'UTF-8'))
    p.stdin.close()
    return p.stdout.read().decode("UTF-8")

input_cabecera = replaceFont(input_cabecera)
input_cuerpo = replaceFont(input_cuerpo)
input_pie = replaceFont(input_pie)

# Replace pict
def replacePict(aString):
    p = subprocess.Popen(['./replacepict'], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
    p.stdin.write(bytes(aString, 'UTF-8'))
    p.stdin.close()
    return p.stdout.read().decode("UTF-8")

input_cabecera = replacePict(input_cabecera)
input_cuerpo = replacePict(input_cuerpo)
input_pie = replacePict(input_pie)

def save(filename, content):
    text_file = open(filename, "w")
    text_file.write(content)
    text_file.close()

save("cabecera.rtf", input_cabecera)
save("cuerpo.rtf", input_cuerpo)
save("pie.rtf", input_pie)
