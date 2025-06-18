export const manifest = (() => {
function __memo(fn) {
	let value;
	return () => value ??= (value = fn());
}

return {
	appDir: "_app",
	appPath: "_app",
	assets: new Set([".ic-assets.json5",".well-known/ic-domains","favicon.ico","logo2.svg"]),
	mimeTypes: {".json5":"application/json5",".svg":"image/svg+xml"},
	_: {
		client: {start:"_app/immutable/entry/start.BfXdODSE.js",app:"_app/immutable/entry/app.BV_3gxrL.js",imports:["_app/immutable/entry/start.BfXdODSE.js","_app/immutable/chunks/DNnBoE2k.js","_app/immutable/chunks/B39EYcLZ.js","_app/immutable/chunks/3-1D8cKG.js","_app/immutable/entry/app.BV_3gxrL.js","_app/immutable/chunks/B39EYcLZ.js","_app/immutable/chunks/pUpZq1fU.js","_app/immutable/chunks/3-1D8cKG.js","_app/immutable/chunks/D52fYbM_.js"],stylesheets:[],fonts:[],uses_env_dynamic_public:false},
		nodes: [
			__memo(() => import('./nodes/0.js')),
			__memo(() => import('./nodes/1.js')),
			__memo(() => import('./nodes/2.js'))
		],
		routes: [
			{
				id: "/",
				pattern: /^\/$/,
				params: [],
				page: { layouts: [0,], errors: [1,], leaf: 2 },
				endpoint: null
			}
		],
		prerendered_routes: new Set([]),
		matchers: async () => {
			
			return {  };
		},
		server_assets: {}
	}
}
})();
