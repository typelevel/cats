#!/bin/bash

# For temporary use while cross-publishing for Scala.js 0.6 and 1.0.

SCALAJS_VERSION=0.6.32 sbt +kernelJS/publishSigned
SCALAJS_VERSION=0.6.32 sbt +kernelLawsJS/publishSigned
SCALAJS_VERSION=0.6.32 sbt +coreJS/publishSigned
SCALAJS_VERSION=0.6.32 sbt +lawsJS/publishSigned
SCALAJS_VERSION=0.6.32 sbt +freeJS/publishSigned
SCALAJS_VERSION=0.6.32 sbt +testkitJS/publishSigned
SCALAJS_VERSION=0.6.32 sbt +alleycatsCoreJS/publishSigned
SCALAJS_VERSION=0.6.32 sbt +alleycatsLawsJS/publishSigned
