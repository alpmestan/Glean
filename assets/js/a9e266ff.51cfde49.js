"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[9085],{3905:function(e,t,n){n.r(t),n.d(t,{MDXContext:function(){return l},MDXProvider:function(){return p},mdx:function(){return h},useMDXComponents:function(){return d},withMDXComponents:function(){return u}});var r=n(67294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function a(){return(a=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e}).apply(this,arguments)}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function c(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,r,o=function(e,t){if(null==e)return{};var n,r,o={},a=Object.keys(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var l=r.createContext({}),u=function(e){return function(t){var n=d(t.components);return r.createElement(e,a({},t,{components:n}))}},d=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):c(c({},t),e)),n},p=function(e){var t=d(e.components);return r.createElement(l.Provider,{value:t},e.children)},f={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,o=e.mdxType,a=e.originalType,i=e.parentName,l=s(e,["components","mdxType","originalType","parentName"]),u=d(n),p=o,m=u["".concat(i,".").concat(p)]||u[p]||f[p]||a;return n?r.createElement(m,c(c({ref:t},l),{},{components:n})):r.createElement(m,c({ref:t},l))}));function h(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=n.length,i=new Array(a);i[0]=m;var c={};for(var s in t)hasOwnProperty.call(t,s)&&(c[s]=t[s]);c.originalType=e,c.mdxType="string"==typeof e?e:o,i[1]=c;for(var l=2;l<a;l++)i[l]=n[l];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},8237:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return s},contentTitle:function(){return l},metadata:function(){return u},toc:function(){return d},default:function(){return f}});var r=n(87462),o=n(63366),a=(n(67294),n(3905)),i=n(44256),c=["components"],s={id:"basic",title:"Basic Concepts",sidebar_label:"Basic Concepts"},l=void 0,u={unversionedId:"schema/basic",id:"schema/basic",isDocsHomePage:!1,title:"Basic Concepts",description:"This section explains the concepts behind how Glean stores data, and",source:"@site/../docs/schema/basic.md",sourceDirName:"schema",slug:"/schema/basic",permalink:"/docs/schema/basic",editUrl:"https://www.internalfb.com/intern/diffusion/FBS/browse/master/fbcode/glean/website/../docs/schema/basic.md",version:"current",frontMatter:{id:"basic",title:"Basic Concepts",sidebar_label:"Basic Concepts"},sidebar:"someSidebar",previous:{title:"Walkthrough",permalink:"/docs/walkthrough"},next:{title:"Built-in Types",permalink:"/docs/schema/types"}},d=[],p={toc:d};function f(e){var t=e.components,n=(0,o.Z)(e,c);return(0,a.mdx)("wrapper",(0,r.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,a.mdx)("p",null,"This section explains the concepts behind how Glean stores data, and\nhow to define a schema to describe your data."),(0,a.mdx)(i.FbInternalOnly,{mdxType:"FbInternalOnly"},(0,a.mdx)("p",null,"For a more formal description, see ",(0,a.mdx)("a",{parentName:"p",href:"https://fb.workplace.com/notes/roman-leshchinskiy/glean-a-little-formally/504151247034061/"},"Glean, a little formally"),".")),(0,a.mdx)("p",null,"A Glean Database consists of a set of ",(0,a.mdx)("strong",{parentName:"p"},(0,a.mdx)("em",{parentName:"strong"},"Facts")),". Facts are unique;\neach fact is stored only once."),(0,a.mdx)("p",null,"The ",(0,a.mdx)("strong",{parentName:"p"},(0,a.mdx)("em",{parentName:"strong"},"Schema"))," describing the database is a set of\n",(0,a.mdx)("strong",{parentName:"p"},(0,a.mdx)("em",{parentName:"strong"},"Predicates")),". You can think of the predicates as the types of the\nfacts. Each fact is an instance of one predicate."),(0,a.mdx)("p",null,"A predicate looks like this:"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre"},"predicate P : KeyType -> ValueType\n")),(0,a.mdx)("p",null,"where"),(0,a.mdx)("ul",null,(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"P")," is the name of the predicate. e.g. ",(0,a.mdx)("inlineCode",{parentName:"li"},"src.File")),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"KeyType")," is the ",(0,a.mdx)("em",{parentName:"li"},"key type")),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"ValueType")," is the ",(0,a.mdx)("em",{parentName:"li"},"value type"))),(0,a.mdx)("p",null,(0,a.mdx)("inlineCode",{parentName:"p"},"KeyType")," and ",(0,a.mdx)("inlineCode",{parentName:"p"},"ValueType")," are types; the type language is described in\n",(0,a.mdx)("a",{parentName:"p",href:"/docs/schema/types"},"Built-in Types"),"."),(0,a.mdx)("p",null,"Every ",(0,a.mdx)("em",{parentName:"p"},"fact")," in predicate P has:"),(0,a.mdx)("ul",null,(0,a.mdx)("li",{parentName:"ul"},"A ",(0,a.mdx)("strong",{parentName:"li"},(0,a.mdx)("em",{parentName:"strong"},"Fact ID")),": a unique 64-bit integer that identifies the fact"),(0,a.mdx)("li",{parentName:"ul"},"A ",(0,a.mdx)("strong",{parentName:"li"},(0,a.mdx)("em",{parentName:"strong"},"Key")),", a term of type ",(0,a.mdx)("inlineCode",{parentName:"li"},"KeyType")),(0,a.mdx)("li",{parentName:"ul"},"A ",(0,a.mdx)("strong",{parentName:"li"},(0,a.mdx)("em",{parentName:"strong"},"Value")),", a term of type ",(0,a.mdx)("inlineCode",{parentName:"li"},"ValueType"))),(0,a.mdx)("p",null,'(we use "term" here to avoid confusion with the use of "value" in a predicate).'),(0,a.mdx)("p",null,"Often the value is empty, represented by the empty record ",(0,a.mdx)("inlineCode",{parentName:"p"},"{}"),". Most\npredicates have all their information in the key."),(0,a.mdx)("p",null,"You can think of Glean as like a key/value store: we can look up a key\n",(0,a.mdx)("inlineCode",{parentName:"p"},"K")," in predicate ",(0,a.mdx)("inlineCode",{parentName:"p"},"P")," and get back value ",(0,a.mdx)("inlineCode",{parentName:"p"},"V.")," We can also query for\n",(0,a.mdx)("em",{parentName:"p"},"patterns")," that match multiple keys, and get back all the facts that\nmatch the pattern. More about this when we talk about ",(0,a.mdx)("a",{parentName:"p",href:"/docs/angle/intro"},"Angle"),"\nqueries."))}f.isMDXComponent=!0},47596:function(e,t,n){var r=this&&this.__awaiter||function(e,t,n,r){return new(n||(n=Promise))((function(o,a){function i(e){try{s(r.next(e))}catch(t){a(t)}}function c(e){try{s(r.throw(e))}catch(t){a(t)}}function s(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(i,c)}s((r=r.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.getSpecInfo=void 0;const o=n(11737);t.getSpecInfo=function(e){return r(this,void 0,void 0,(function*(){return yield o.call({module:"bloks",api:"getSpecInfo",args:{styleId:e}})}))}},11737:function(e,t){var n=this&&this.__awaiter||function(e,t,n,r){return new(n||(n=Promise))((function(o,a){function i(e){try{s(r.next(e))}catch(t){a(t)}}function c(e){try{s(r.throw(e))}catch(t){a(t)}}function s(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(i,c)}s((r=r.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.call=void 0;let r=!1,o=0;const a={};t.call=function(e){return n(this,void 0,void 0,(function*(){if("staticdocs.thefacebook.com"!==window.location.hostname&&"localhost"!==window.location.hostname)return Promise.reject(new Error("Not running on static docs"));r||(r=!0,window.addEventListener("message",(e=>{if("static-docs-bridge-response"!==e.data.event)return;const t=e.data.id;t in a||console.error(`Recieved response for id: ${t} with no matching receiver`),"response"in e.data?a[t].resolve(e.data.response):a[t].reject(new Error(e.data.error)),delete a[t]})));const t=o++,n=new Promise(((e,n)=>{a[t]={resolve:e,reject:n}})),i={event:"static-docs-bridge-call",id:t,module:e.module,api:e.api,args:e.args},c="localhost"===window.location.hostname?"*":"https://www.internalfb.com";return window.parent.postMessage(i,c),n}))}},24855:function(e,t,n){var r=this&&this.__awaiter||function(e,t,n,r){return new(n||(n=Promise))((function(o,a){function i(e){try{s(r.next(e))}catch(t){a(t)}}function c(e){try{s(r.throw(e))}catch(t){a(t)}}function s(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(i,c)}s((r=r.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.reportFeatureUsage=t.reportContentCopied=void 0;const o=n(11737);t.reportContentCopied=function(e){return r(this,void 0,void 0,(function*(){const{textContent:t}=e;try{yield o.call({module:"feedback",api:"reportContentCopied",args:{textContent:t}})}catch(n){}}))},t.reportFeatureUsage=function(e){return r(this,void 0,void 0,(function*(){const{featureName:t,id:n}=e;console.log("used feature");try{yield o.call({module:"feedback",api:"reportFeatureUsage",args:{featureName:t,id:n}})}catch(r){}}))}},44256:function(e,t,n){var r=this&&this.__createBinding||(Object.create?function(e,t,n,r){void 0===r&&(r=n),Object.defineProperty(e,r,{enumerable:!0,get:function(){return t[n]}})}:function(e,t,n,r){void 0===r&&(r=n),e[r]=t[n]}),o=this&&this.__setModuleDefault||(Object.create?function(e,t){Object.defineProperty(e,"default",{enumerable:!0,value:t})}:function(e,t){e.default=t}),a=this&&this.__importStar||function(e){if(e&&e.__esModule)return e;var t={};if(null!=e)for(var n in e)"default"!==n&&Object.prototype.hasOwnProperty.call(e,n)&&r(t,e,n);return o(t,e),t};Object.defineProperty(t,"__esModule",{value:!0}),t.OssOnly=t.FbInternalOnly=t.isInternal=t.validateFbContentArgs=t.fbInternalOnly=t.fbContent=t.inpageeditor=t.feedback=t.uidocs=t.bloks=void 0,t.bloks=a(n(47596)),t.uidocs=a(n(17483)),t.feedback=a(n(24855)),t.inpageeditor=a(n(27312));const i=["internal","external"];function c(e){return l(e),u()?"internal"in e?s(e.internal):[]:"external"in e?s(e.external):[]}function s(e){return"function"==typeof e?e():e}function l(e){if("object"!=typeof e)throw new Error(`fbContent() args must be an object containing keys: ${i}. Instead got ${e}`);if(!Object.keys(e).find((e=>i.find((t=>t===e)))))throw new Error(`No valid args found in ${JSON.stringify(e)}. Accepted keys: ${i}`);const t=Object.keys(e).filter((e=>!i.find((t=>t===e))));if(t.length>0)throw new Error(`Unexpected keys ${t} found in fbContent() args. Accepted keys: ${i}`)}function u(){try{return Boolean(!1)}catch(e){return console.log("process.env.FB_INTERNAL couldn't be read, maybe you forgot to add the required webpack EnvironmentPlugin config?",e),!1}}t.fbContent=c,t.fbInternalOnly=function(e){return c({internal:e})},t.validateFbContentArgs=l,t.isInternal=u,t.FbInternalOnly=function(e){return u()?e.children:null},t.OssOnly=function(e){return u()?null:e.children}},27312:function(e,t,n){var r=this&&this.__awaiter||function(e,t,n,r){return new(n||(n=Promise))((function(o,a){function i(e){try{s(r.next(e))}catch(t){a(t)}}function c(e){try{s(r.throw(e))}catch(t){a(t)}}function s(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(i,c)}s((r=r.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.submitDiff=void 0;const o=n(11737);t.submitDiff=function(e){return r(this,void 0,void 0,(function*(){const{file_path:t,new_content:n,project_name:r}=e;try{return yield o.call({module:"inpageeditor",api:"createPhabricatorDiffApi",args:{file_path:t,new_content:n,project_name:r}})}catch(a){throw new Error(`Error occurred while trying to submit diff. Stack trace: ${a}`)}}))}},17483:function(e,t,n){var r=this&&this.__awaiter||function(e,t,n,r){return new(n||(n=Promise))((function(o,a){function i(e){try{s(r.next(e))}catch(t){a(t)}}function c(e){try{s(r.throw(e))}catch(t){a(t)}}function s(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(i,c)}s((r=r.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.getApi=t.docsets=void 0;const o=n(11737);t.docsets={BLOKS_CORE:"887372105406659"},t.getApi=function(e){return r(this,void 0,void 0,(function*(){const{name:t,framework:n,docset:r}=e;return yield o.call({module:"uidocs",api:"getApi",args:{name:t,framework:n,docset:r}})}))}}}]);