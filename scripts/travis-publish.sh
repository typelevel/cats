#!/bin/bash


# Build Overview:
# The overall build is split into a number of parts
# 1. The build for coverage is performed. This:
#   a. First enables the coverage processing, and then
#   b. Builds and tests for the JVM using the validateJVM target, and then
#   c. Produces the coverage report, and then
#   d. Clean is run (as part of coverageReport), to clear down the built artifacts
# 2. The Scalafix subdirectory is run, executing the tests inside.
# 3. The Scala.js build is executed, compiling the application and testing it.
# 4. The validateJVM target is executed again, due to the fact that producing coverage with the
#    code coverage tool causes the byte code to be instrumented/modified to record the coverage
#    metrics when the tests are executing. This causes the full JVM build to be run a second time.

# Example setting to use at command line for testing:
# export TRAVIS_SCALA_VERSION=2.10.5;export TRAVIS_PULL_REQUEST="false";export TRAVIS_BRANCH="main"


sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

export publish_cmd="publishLocal"

if [[ $TRAVIS_PULL_REQUEST == "false" && $TRAVIS_BRANCH == "main" && $(cat version.sbt) =~ "-SNAPSHOT" ]]; then
  export publish_cmd="publish gitSnapshots publish"
  # temporarily disable to stabilize travis
  #if [[ $TRAVIS_SCALA_VERSION =~ ^2\.11\. ]]; then
  #  export publish_cmd="publishMicrosite"
  #fi
fi


export COURSIER_VERBOSITY=0

core_js="$sbt_cmd validateJS"
kernel_js="$sbt_cmd validateKernelJS"
free_js="$sbt_cmd validateFreeJS"

js="$core_js && $free_js && $kernel_js"

# Skip coverage and docs on 2.13 for now.
if [[ $TRAVIS_SCALA_VERSION == *"2.13"* ]]; then
jvm="$sbt_cmd buildJVM"
else
jvm="$sbt_cmd coverage validateJVM coverageReport && codecov"
fi



if [[ $TRAVIS_SCALA_VERSION == *"2.12"* ]]; then
scalafix="cd scalafix && sbt tests/test && cd .. &&"
else
scalafix=""
fi

if [[ $JS_BUILD == "true" ]]; then
run_cmd="$js"
else
run_cmd="$scalafix $jvm && $sbt_cmd $publish_cmd"
fi

eval $run_cmd
