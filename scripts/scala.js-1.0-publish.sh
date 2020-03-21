#!/bin/bash

# For temporary use while cross-publishing for Scala.js 0.6 and 1.0.

SCALAJS_VERSION=1.0.0 sbt +macrosJS/publishSigned
SCALAJS_VERSION=1.0.0 sbt +kernelJS/publishSigned
SCALAJS_VERSION=1.0.0 sbt +kernelLawsJS/publishSigned
SCALAJS_VERSION=1.0.0 sbt +coreJS/publishSigned
SCALAJS_VERSION=1.0.0 sbt +lawsJS/publishSigned
SCALAJS_VERSION=1.0.0 sbt +freeJS/publishSigned
SCALAJS_VERSION=1.0.0 sbt +testkitJS/publishSigned
SCALAJS_VERSION=1.0.0 sbt +alleycatsCoreJS/publishSigned
SCALAJS_VERSION=1.0.0 sbt +alleycatsLawsJS/publishSigned
