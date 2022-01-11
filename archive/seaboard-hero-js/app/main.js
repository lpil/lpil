'use strict';

import initDevice from 'device';

import deviceController from 'controllers/device_controller';

initDevice(deviceController);

export default () => console.log('Hello, world');
