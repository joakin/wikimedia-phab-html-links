var _rollbarConfig = {
    accessToken: "27b79e649bfd41389e4d17a48dfb8805",
    captureUncaught: true,
    payload: {
        environment: process.env.NODE_ENV || 'development'
    }
};

// From https://github.com/rollbar/rollbar.js/
var Rollbar = require('./vendor/rollbar.umd.nojson.min.js').init(_rollbarConfig);
