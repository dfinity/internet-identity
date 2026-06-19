import * as client_hooks from '../../../src/hooks.client.ts';
import * as universal_hooks from '../../../src/hooks.ts';

export { matchers } from './matchers.js';

export const nodes = [
	() => import('./nodes/0'),
	() => import('./nodes/1'),
	() => import('./nodes/2'),
	() => import('./nodes/3'),
	() => import('./nodes/4'),
	() => import('./nodes/5'),
	() => import('./nodes/6'),
	() => import('./nodes/7'),
	() => import('./nodes/8'),
	() => import('./nodes/9'),
	() => import('./nodes/10'),
	() => import('./nodes/11'),
	() => import('./nodes/12'),
	() => import('./nodes/13'),
	() => import('./nodes/14'),
	() => import('./nodes/15'),
	() => import('./nodes/16'),
	() => import('./nodes/17'),
	() => import('./nodes/18'),
	() => import('./nodes/19'),
	() => import('./nodes/20'),
	() => import('./nodes/21'),
	() => import('./nodes/22'),
	() => import('./nodes/23'),
	() => import('./nodes/24')
];

export const server_loads = [];

export const dictionary = {
		"/(new-styling)": [7,[2]],
		"/(new-styling)/about": [8,[2]],
		"/(new-styling)/activate": [9,[2]],
		"/(new-styling)/authorize": [10,[2,3]],
		"/(new-styling)/callback": [11,[2]],
		"/(new-styling)/cli": [12,[2,4]],
		"/iframe/webauthn": [23],
		"/(new-styling)/login": [13,[2]],
		"/(new-styling)/manage/(authenticated)/(home)": [14,[2,5]],
		"/(new-styling)/manage/(authenticated)/access": [15,[2,5]],
		"/(new-styling)/manage/(authenticated)/recovery": [16,[2,5]],
		"/(new-styling)/manage/(authenticated)/settings": [17,[2,5]],
		"/(new-styling)/mcp": [18,[2,6]],
		"/(new-styling)/pair": [19,[2]],
		"/(new-styling)/recovery": [20,[2]],
		"/(new-styling)/self-service": [21,[2]],
		"/(new-styling)/unsupported": [22,[2]],
		"/vc-flow/index": [24]
	};

export const hooks = {
	handleError: client_hooks.handleError || (({ error }) => { console.error(error) }),
	init: client_hooks.init,
	reroute: universal_hooks.reroute || (() => {}),
	transport: universal_hooks.transport || {}
};

export const decoders = Object.fromEntries(Object.entries(hooks.transport).map(([k, v]) => [k, v.decode]));
export const encoders = Object.fromEntries(Object.entries(hooks.transport).map(([k, v]) => [k, v.encode]));

export const hash = false;

export const decode = (type, value) => decoders[type](value);

export { default as root } from '../root.js';