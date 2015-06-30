'use strict';

angular.module('myapp')
  .factory('Test', ['$resource', function ($resource) {
    return $resource('myapp/Tests/:id', {}, {
      'query': { method: 'GET', isArray: true},
      'get': { method: 'GET'},
      'update': { method: 'PUT'}
    });
  }]);
