#!groovy
// -*- mode: groovy -*-

build('swag-api-keys', 'docker-host') {
  checkoutRepo()
  loadBuildUtils('build-utils')

  def pipeDefault
  def withWsCache
  runStage('load pipeline') {
    env.JENKINS_LIB = "build-utils/jenkins_lib"
    pipeDefault = load("${env.JENKINS_LIB}/pipeDefault.groovy")
    withWsCache = load("${env.JENKINS_LIB}/withWsCache.groovy")
  }

  pipeDefault() {

    runStage('install-deps') {
      withWsCache("node_modules") {
        sh 'make wc_install'
      }
    }

    runStage('validate-spec') {
      sh 'make wc_validate'
    }

  }
}
