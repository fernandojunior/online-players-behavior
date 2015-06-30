'use strict';

angular.module('myapp')
  .config(['$routeProvider', function ($routeProvider) {
    $routeProvider
      .when('/Tests', {
        templateUrl: 'views/Test/Tests.html',
        controller: 'TestController',
        resolve:{
          resolvedTest: ['Test', function (Test) {
            return Test.query();
          }]
        }
      })
    }]);
