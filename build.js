#!/usr/bin/env node
'use strict';
var Path = require('path');

var TARGET_DIR = 'dist'
if (process.argv[2]) {
    TARGET_DIR = process.argv[2]
}

require('shelljs/global');
set('-e');

mkdir('-p', TARGET_DIR)

// cp('-R', 'web/*', TARGET_DIR + '/');

exec('npm run openapi bundle -- -o ' + Path.join(TARGET_DIR, 'openapi') + ' --ext json');
exec('npm run openapi bundle -- -o ' + Path.join(TARGET_DIR, 'openapi') + ' --ext yaml');

exec([
    'npm run openapi build-docs --',
    '--cdn',
    '--template web/template.hbs',
    '-o', Path.join(TARGET_DIR, 'index.html'),
    Path.join(TARGET_DIR, 'openapi.yaml')
].join(' '));

var SWAGGER_UI_DIST = Path.dirname(require.resolve('swagger-ui-dist'));
var SWAGGER_UI_DIR = Path.join(TARGET_DIR, 'swagger-ui');
rm('-rf', SWAGGER_UI_DIR);
cp('-R', SWAGGER_UI_DIST, SWAGGER_UI_DIR);
sed('-i', 'https://petstore.swagger.io/v2/swagger.json', '../openapi.json', TARGET_DIR + '/swagger-ui/index.html');
