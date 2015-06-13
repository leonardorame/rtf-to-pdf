# Se utilizara una plantilla de libreoffice, la cual debera tener si o si activado dentro de las propiedades del header autofit en true.
# Format -> Page -> Header -> AutoFit = checked

import io
#import StringIO

import os
import sys

import uno
import unohelper
import string

import json
import base64

import subprocess
from com.sun.star.beans import PropertyValue
from unohelper import Base
from com.sun.star.io import IOException, XOutputStream, XInputStream, XSeekable

currDir = os.path.dirname( os.path.realpath(__file__) )

# Se obtiene el documento por stdin.
input = ""
input_pie = ""
input_cuerpo = ""
input_cabecera = ""
input_lotemplate = ""
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
open('template.ott', 'wb').write(base64.b64decode(input['lotemplate']))

# Replace font
def replaceFont(aString):
    p = subprocess.Popen(['./replacefont', 'Lucida Console', '21', '400'], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
    p.stdin.write(bytes(aString, 'UTF-8'))
    p.stdin.close()
    return p.stdout.read().decode("UTF-8")

input_cabecera = replaceFont(input_cabecera)
input_cuerpo = replaceFont(input_cuerpo)
input_pie = replaceFont(input_pie)

# Some times, pictures are hughe, hence 
# we use the replacepict program to 
# convert to the correct size. Also
# some times pictures in dibitmap format 
# are't handled by libreoffice, so
# replacepict converts them to jpeg

def replacePict(aString):
    p = subprocess.Popen(['./replacepict'], stdout=subprocess.PIPE, stdin=subprocess.PIPE)
    p.stdin.write(bytes(aString, 'UTF-8'))
    p.stdin.close()
    return p.stdout.read().decode("UTF-8")

input_cabecera = replacePict(input_cabecera)
input_cuerpo = replacePict(input_cuerpo)
input_pie = replacePict(input_pie)

text_file = open("pie.rtf", "w")
text_file.write(input_pie)
text_file.close()

#convertimos los strings a un stream
input_cabecera_Stream = io.StringIO(input_cabecera)
input_cuerpo_Stream = io.StringIO(input_cuerpo)
input_pie_Stream = io.StringIO(input_pie)

# Nos conectamos a libreoffice 
localContext = uno.getComponentContext()
resolver = localContext.ServiceManager.createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", localContext )
ctx = resolver.resolve( "uno:socket,host=127.0.0.1,port=2002;urp;StarOffice.ComponentContext" )
smgr = ctx.ServiceManager
desktop = smgr.createInstanceWithContext( "com.sun.star.frame.Desktop",ctx)

# plantilla-original.ott, es un documento de libreoffice que se encuentra en blanco, para el proposito especifico posee solo encabezado (no requeria el uso de pie). 
plantilla = currDir + "/plantilla-original.ott";
oTemplateProperties = (
    PropertyValue( "AsTemplate" , 0, True , 0 ),
    )

document = desktop.loadComponentFromURL("file://" + plantilla, "_blank", 0, oTemplateProperties)
#document = desktop.loadComponentFromURL("private:factory/swriter", "_blank", 0, ())

class InputStream(unohelper.Base, XInputStream, XSeekable):
    """ Minimal Implementation of XInputStream """
    def __init__(self, inStream):
        self.stream = inStream
        self.stream.seek(0, os.SEEK_END)
        self.size = self.stream.tell()
       
    def readBytes(self, retSeq, nByteCount):
        retSeq = self.stream.read(nByteCount)
        return (len(retSeq), uno.ByteSequence(retSeq))
   
    def readSomeBytes(self, foo, n):
        return self.readBytes(foo, n)

    def skipBytes(self, n):
        self.stream.seek (n, 1)

    def available(self):
        return self.size - self.stream.tell();

    def closeInput(self):
        self.stream.close()

    def seek(self, posn):
        self.stream.seek(int(posn))

    def getPosition(self):
        return int(self.stream.tell())

    def getLength(self):
        return int(self.size)

def insertStream(aStream):
    text = document.Text
    cursor = text.createTextCursor()
    inPropsce = (
        PropertyValue( "FilterName" , 0, "Rich Text Format" , 0 ),
        PropertyValue( "InputStream", 0, InputStream(aStream), 0)
        )
    cursor.insertDocumentFromURL("private:stream", inPropsce)
    text.insertControlCharacter(cursor, uno.getConstantByName("com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK"), 0)
    #cursor.gotoEnd(False)

# se insertan en orden reverso
insertStream(input_pie_Stream)
insertStream(input_cuerpo_Stream)
insertStream(input_cabecera_Stream)

class OutputStream( Base, XOutputStream ):
    def __init__( self ):
        self.closed = 0
    def closeOutput(self):
        self.closed = 1
    def writeBytes( self, seq ):
        sys.stdout.buffer.write( seq.value )
    def flush( self ):
        pass

filterName = "writer_pdf_Export"

outProps = (
    PropertyValue( "FilterName" , 0, filterName , 0 ),
    PropertyValue( "Overwrite" , 0, True , 0 ),
    PropertyValue( "OutputStream", 0, OutputStream(), 0)
    )
	   
try:
    document.storeToURL("private:stream", outProps)
    document.dispose();
    sys.exit(0);
except IOException as e:
    sys.stderr.write("Error: " + e.Message)
