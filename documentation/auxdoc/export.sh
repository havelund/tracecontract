#!/bin/bash

# Creating Scala documentation:

scaladoc -d doc src/tracecontract/*.scala
cp -r extra doc/extra

# Creating jar file:

cd bin
jar -cfv tracecontract.jar tracecontract
cd ..

# Package doc and jar file into one zip file:                                                                                                                                
mkdir tracecontractlib
cp -r doc tracecontractlib/doc
mv bin/tracecontract.jar tracecontractlib

rm tracecontractlib.zip
zip -r tracecontractlib.zip tracecontractlib
