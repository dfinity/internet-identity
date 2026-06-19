
// this file is generated — do not edit it


declare module "svelte/elements" {
	export interface HTMLAttributes<T> {
		'data-sveltekit-keepfocus'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-noscroll'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-preload-code'?:
			| true
			| ''
			| 'eager'
			| 'viewport'
			| 'hover'
			| 'tap'
			| 'off'
			| undefined
			| null;
		'data-sveltekit-preload-data'?: true | '' | 'hover' | 'tap' | 'off' | undefined | null;
		'data-sveltekit-reload'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-replacestate'?: true | '' | 'off' | undefined | null;
	}
}

export {};


declare module "$app/types" {
	export interface AppTypes {
		RouteId(): "/(new-styling)" | "/" | "/(new-styling)/about" | "/(new-styling)/activate" | "/(new-styling)/authorize" | "/(new-styling)/authorize/views" | "/(new-styling)/callback" | "/(new-styling)/cli" | "/(new-styling)/cli/components" | "/(new-styling)/cli/views" | "/iframe" | "/iframe/webauthn" | "/(new-styling)/login" | "/(new-styling)/manage/(authenticated)/(home)" | "/(new-styling)/manage/(authenticated)" | "/(new-styling)/manage" | "/(new-styling)/manage/(authenticated)/access" | "/(new-styling)/manage/(authenticated)/access/components" | "/(new-styling)/manage/(authenticated)/recovery" | "/(new-styling)/manage/(authenticated)/recovery/components" | "/(new-styling)/manage/(authenticated)/settings" | "/(new-styling)/manage/(authenticated)/settings/components" | "/(new-styling)/mcp" | "/(new-styling)/mcp/components" | "/(new-styling)/mcp/views" | "/(new-styling)/pair" | "/(new-styling)/recovery" | "/(new-styling)/self-service" | "/(new-styling)/unsupported" | "/(new-styling)/unsupported/components" | "/(new-styling)/unsupported/components/ui" | "/(new-styling)/unsupported/components/views" | "/vc-flow" | "/vc-flow/index";
		RouteParams(): {
			
		};
		LayoutParams(): {
			"/(new-styling)": Record<string, never>;
			"/": Record<string, never>;
			"/(new-styling)/about": Record<string, never>;
			"/(new-styling)/activate": Record<string, never>;
			"/(new-styling)/authorize": Record<string, never>;
			"/(new-styling)/authorize/views": Record<string, never>;
			"/(new-styling)/callback": Record<string, never>;
			"/(new-styling)/cli": Record<string, never>;
			"/(new-styling)/cli/components": Record<string, never>;
			"/(new-styling)/cli/views": Record<string, never>;
			"/iframe": Record<string, never>;
			"/iframe/webauthn": Record<string, never>;
			"/(new-styling)/login": Record<string, never>;
			"/(new-styling)/manage/(authenticated)/(home)": Record<string, never>;
			"/(new-styling)/manage/(authenticated)": Record<string, never>;
			"/(new-styling)/manage": Record<string, never>;
			"/(new-styling)/manage/(authenticated)/access": Record<string, never>;
			"/(new-styling)/manage/(authenticated)/access/components": Record<string, never>;
			"/(new-styling)/manage/(authenticated)/recovery": Record<string, never>;
			"/(new-styling)/manage/(authenticated)/recovery/components": Record<string, never>;
			"/(new-styling)/manage/(authenticated)/settings": Record<string, never>;
			"/(new-styling)/manage/(authenticated)/settings/components": Record<string, never>;
			"/(new-styling)/mcp": Record<string, never>;
			"/(new-styling)/mcp/components": Record<string, never>;
			"/(new-styling)/mcp/views": Record<string, never>;
			"/(new-styling)/pair": Record<string, never>;
			"/(new-styling)/recovery": Record<string, never>;
			"/(new-styling)/self-service": Record<string, never>;
			"/(new-styling)/unsupported": Record<string, never>;
			"/(new-styling)/unsupported/components": Record<string, never>;
			"/(new-styling)/unsupported/components/ui": Record<string, never>;
			"/(new-styling)/unsupported/components/views": Record<string, never>;
			"/vc-flow": Record<string, never>;
			"/vc-flow/index": Record<string, never>
		};
		Pathname(): "/" | "/about" | "/activate" | "/authorize" | "/callback" | "/cli" | "/iframe/webauthn" | "/login" | "/manage" | "/manage/access" | "/manage/recovery" | "/manage/settings" | "/mcp" | "/pair" | "/recovery" | "/self-service" | "/unsupported" | "/vc-flow/index";
		ResolvedPathname(): `${"" | `/${string}`}${ReturnType<AppTypes['Pathname']>}`;
		Asset(): "/faq.html" | "/favicon.svg" | "/robots.txt" | "/sitemap.xml" | "/social-image.png" | string & {};
	}
}