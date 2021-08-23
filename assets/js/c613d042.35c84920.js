"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[7134],{3905:function(e,t,n){n.r(t),n.d(t,{MDXContext:function(){return c},MDXProvider:function(){return m},mdx:function(){return f},useMDXComponents:function(){return p},withMDXComponents:function(){return s}});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(){return(i=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var a in n)Object.prototype.hasOwnProperty.call(n,a)&&(e[a]=n[a])}return e}).apply(this,arguments)}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function d(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var c=a.createContext({}),s=function(e){return function(t){var n=p(t.components);return a.createElement(e,i({},t,{components:n}))}},p=function(e){var t=a.useContext(c),n=t;return e&&(n="function"==typeof e?e(t):d(d({},t),e)),n},m=function(e){var t=p(e.components);return a.createElement(c.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},h=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,i=e.originalType,o=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),s=p(n),m=r,h=s["".concat(o,".").concat(m)]||s[m]||u[m]||i;return n?a.createElement(h,d(d({ref:t},c),{},{components:n})):a.createElement(h,d({ref:t},c))}));function f(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=n.length,o=new Array(i);o[0]=h;var d={};for(var l in t)hasOwnProperty.call(t,l)&&(d[l]=t[l]);d.originalType=e,d.mdxType="string"==typeof e?e:r,o[1]=d;for(var c=2;c<i;c++)o[c]=n[c];return a.createElement.apply(null,o)}return a.createElement.apply(null,n)}h.displayName="MDXCreateElement"},46168:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return l},contentTitle:function(){return c},metadata:function(){return s},toc:function(){return p},default:function(){return u}});var a=n(87462),r=n(63366),i=(n(67294),n(3905)),o=n(44256),d=["components"],l={id:"derived",title:"Derived Predicates",sidebar_label:"Derived Predicates"},c=void 0,s={unversionedId:"derived",id:"derived",isDocsHomePage:!1,title:"Derived Predicates",description:'Glean supports predicates that are defined in terms of a query. There are two types of derived predicates, "stored" and "on demand".',source:"@site/../docs/derived.md",sourceDirName:".",slug:"/derived",permalink:"/docs/derived",editUrl:"https://www.internalfb.com/intern/diffusion/FBS/browse/master/fbcode/glean/website/../docs/derived.md",version:"current",frontMatter:{id:"derived",title:"Derived Predicates",sidebar_label:"Derived Predicates"},sidebar:"someSidebar",previous:{title:"Haskell",permalink:"/docs/query/api/haskell"},next:{title:"Glean Databases",permalink:"/docs/databases"}},p=[{value:"Stored derived predicates",id:"stored-derived-predicates",children:[{value:"When do the facts get computed and stored?",id:"when-do-the-facts-get-computed-and-stored",children:[]},{value:"Deriving multiple predicates",id:"deriving-multiple-predicates",children:[]}]},{value:"On-demand derived predicates",id:"on-demand-derived-predicates",children:[]},{value:"Derived predicates for schema migration",id:"derived-predicates-for-schema-migration",children:[{value:"Default derived predicates",id:"default-derived-predicates",children:[]}]},{value:"How do I write and test a derived predicate?",id:"how-do-i-write-and-test-a-derived-predicate",children:[]},{value:"How do I make a derived predicate available?",id:"how-do-i-make-a-derived-predicate-available",children:[]}],m={toc:p};function u(e){var t=e.components,n=(0,r.Z)(e,d);return(0,i.mdx)("wrapper",(0,a.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,i.mdx)("p",null,'Glean supports predicates that are defined in terms of a query. There are two types of derived predicates, "stored" and "on demand".'),(0,i.mdx)("h2",{id:"stored-derived-predicates"},"Stored derived predicates"),(0,i.mdx)("p",null,"For example:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=angle"},"predicate OutTarget.1 :\n    {\n        file : src.File,\n        target : Target,\n    }\n    stored {F,T} where TargetOut {T,F}\n")),(0,i.mdx)("p",null,"This is a schema for a predicate ",(0,i.mdx)("inlineCode",{parentName:"p"},"OutTarget")," with a key type as usual. But unlike a regular predicate, facts of this predicate are not generated by an indexer, instead they are generated by the query given on the final line."),(0,i.mdx)("p",null,"The keyword ",(0,i.mdx)("inlineCode",{parentName:"p"},"stored")," tells Glean that the facts for this predicate will be stored in the database. Omitting the ",(0,i.mdx)("inlineCode",{parentName:"p"},"stored")," keyword indicates that you want the facts to be generated on demand; more about this in ",(0,i.mdx)("a",{parentName:"p",href:"#on-demand-derived-predicates"},"On-demand derived predicates")," below."),(0,i.mdx)("p",null,"You can read the query as"),(0,i.mdx)("blockquote",null,(0,i.mdx)("p",{parentName:"blockquote"},"There is a fact OutTarget {F,T} for every fact TargetOut {T,F}")),(0,i.mdx)("p",null,"The query can be any arbitrary Angle query; the syntax is described in\n",(0,i.mdx)("a",{parentName:"p",href:"/docs/angle/guide"},"Angle Guide"),". The only requirement is that the values\nproduced by the query must match the key type of the predicate being\ndefined."),(0,i.mdx)("p",null,"Why is this useful?  Well, the predicate ",(0,i.mdx)("inlineCode",{parentName:"p"},"TargetOut")," is defined like this:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=angle"},"predicate TargetOut.1 :\n    {\n        target : Target,\n        file : src.File,\n    }\n")),(0,i.mdx)("p",null,"This is a mapping from ",(0,i.mdx)("inlineCode",{parentName:"p"},"Target")," to ",(0,i.mdx)("inlineCode",{parentName:"p"},"File")," (see ",(0,i.mdx)("a",{parentName:"p",href:"angle/efficiency#efficient-matching-of-facts"},"Efficient matching of facts"),").  If we want the reverse mapping, from ",(0,i.mdx)("inlineCode",{parentName:"p"},"File")," to ",(0,i.mdx)("inlineCode",{parentName:"p"},"Target"),", we need a predicate with the fields in the other order, which is exactly what ",(0,i.mdx)("inlineCode",{parentName:"p"},"OutTarget")," is. But it would be laborious to write actual code to generate and store these facts in the database, so Glean allows us to define ",(0,i.mdx)("inlineCode",{parentName:"p"},"OutTarget")," directly in terms of a query, and it will automatically compute the facts of ",(0,i.mdx)("inlineCode",{parentName:"p"},"OutTarget")," and store them in the database."),(0,i.mdx)("h3",{id:"when-do-the-facts-get-computed-and-stored"},"When do the facts get computed and stored?"),(0,i.mdx)("p",null,"Using the ",(0,i.mdx)("inlineCode",{parentName:"p"},"glean")," command-line tool, you direct the server to compute and store the facts for a predicate like this:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=shell"},"glean --service <write-server> derive buck.TargetOut\n")),(0,i.mdx)("p",null,"Replacing ",(0,i.mdx)("inlineCode",{parentName:"p"},"<write-server>")," with the appropriate name of the write\nservice you're using, and replace ",(0,i.mdx)("inlineCode",{parentName:"p"},"buck.TargetOut")," with the name of\nthe predicate you want to derive."),(0,i.mdx)("p",null,"This may take some time, depending on how many facts need to be computed and stored."),(0,i.mdx)("div",{className:"admonition admonition-note alert alert--secondary"},(0,i.mdx)("div",{parentName:"div",className:"admonition-heading"},(0,i.mdx)("h5",{parentName:"div"},(0,i.mdx)("span",{parentName:"h5",className:"admonition-icon"},(0,i.mdx)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"14",height:"16",viewBox:"0 0 14 16"},(0,i.mdx)("path",{parentName:"svg",fillRule:"evenodd",d:"M6.3 5.69a.942.942 0 0 1-.28-.7c0-.28.09-.52.28-.7.19-.18.42-.28.7-.28.28 0 .52.09.7.28.18.19.28.42.28.7 0 .28-.09.52-.28.7a1 1 0 0 1-.7.3c-.28 0-.52-.11-.7-.3zM8 7.99c-.02-.25-.11-.48-.31-.69-.2-.19-.42-.3-.69-.31H6c-.27.02-.48.13-.69.31-.2.2-.3.44-.31.69h1v3c.02.27.11.5.31.69.2.2.42.31.69.31h1c.27 0 .48-.11.69-.31.2-.19.3-.42.31-.69H8V7.98v.01zM7 2.3c-3.14 0-5.7 2.54-5.7 5.68 0 3.14 2.56 5.7 5.7 5.7s5.7-2.55 5.7-5.7c0-3.15-2.56-5.69-5.7-5.69v.01zM7 .98c3.86 0 7 3.14 7 7s-3.14 7-7 7-7-3.12-7-7 3.14-7 7-7z"}))),"note")),(0,i.mdx)("div",{parentName:"div",className:"admonition-content"},(0,i.mdx)("p",{parentName:"div"},"Remember to do this ",(0,i.mdx)("em",{parentName:"p"},"before")," using ",(0,i.mdx)("inlineCode",{parentName:"p"},"glean finish")," to mark the database\nas finished."))),(0,i.mdx)("h3",{id:"deriving-multiple-predicates"},"Deriving multiple predicates"),(0,i.mdx)("p",null,"You can derive multiple predicates together:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=shell"},"glean --service <write-server> derive <predicate> <predicate> ...\n")),(0,i.mdx)("p",null,"But note that these predicates must be independent; they cannot depend\non each other. If you have derived predicates that depend on each\nother, you have to issue separate ",(0,i.mdx)("inlineCode",{parentName:"p"},"glean derive")," commands to derive\nthe predicates in bottom-up dependency order."),(0,i.mdx)("h2",{id:"on-demand-derived-predicates"},"On-demand derived predicates"),(0,i.mdx)("p",null,"The other type of derived predicate is one where the facts are not stored in the database, but are computed on-demand when there is a query for the predicate."),(0,i.mdx)("p",null,"This is useful for a few reasons:"),(0,i.mdx)("ul",null,(0,i.mdx)("li",{parentName:"ul"},"We can support ",(0,i.mdx)("a",{parentName:"li",href:"#derived-predicates-for-schema-migration"},"backwards compatibility")," by defining old predicates in terms of new ones, or forwards compatibility by doing the reverse."),(0,i.mdx)("li",{parentName:"ul"},"We can define queries that extract data and bundle it in a way that's convenient and efficient for the client. This allows clients to avoid fetching more data than they need, for example."),(0,i.mdx)("li",{parentName:"ul"},"Most importantly, we can encapsulate complex queries by defining them in the schema as derived predicates, even building up libraries representing whole abstraction layers over the raw data. Clients can then use the higher-level abstraction regardless of where they're querying from or what language they're using. For a great example of using this in practice, see the ",(0,i.mdx)("a",{parentName:"li",href:"https://github.com/facebookincubator/Glean/blob/master/glean/schema/source/codemarkup.angle"},"codemarkup schema")," that we use to provide a language-neutral abstraction over language-specific schemas.")),(0,i.mdx)("p",null,"For example, in the ",(0,i.mdx)("inlineCode",{parentName:"p"},"cxx")," schema we have a lot of different kinds of declarations. Clients often want to search for a declaration by name, but each of the different declaration kinds has the name in a different place, so this ends up being quite a complicated query from the client side. Using a derived predicate we can easily capture this complexity in one place so that it can be reused by all clients that want to search for declarations by name:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=angle"},"predicate DeclarationWithName :\n    {\n        name : string,\n        decl : Declaration\n    }\n    {Str, Decl} where\n      N = Name Str;\n      Decl =\n        (Declaration (record_ R) where\n          R = RecordDeclaration { name = { name = N }}) |\n        (Declaration (function_ F) where\n          F = FunctionDeclaration { name = { name = { name = N }}}) |\n        # and so on, for all declaration types\n")),(0,i.mdx)("p",null,"Using this predicate requires no magic on the part of the client, they just query for the ",(0,i.mdx)("inlineCode",{parentName:"p"},"cxx1.DeclarationWithName")," predicate in exactly the same way as they would for other predicates, and the Glean query server returns the appropriate facts."),(0,i.mdx)("h2",{id:"derived-predicates-for-schema-migration"},"Derived predicates for schema migration"),(0,i.mdx)("p",null,"One important use case for derived predicates is to make it possible to change the schema without breaking things."),(0,i.mdx)("p",null,"Essentially the idea is that a derived predicate can define the old predicate in terms of the new predicate, providing backwards-compatibility to clients that are expecting to query for the old predicate. Additionally, we might define the new predicate in terms of the old predicate, for forwards-compatibility to allow new clients to work with old data."),(0,i.mdx)("p",null,"Let's work through an example to illustrate the process.  Suppose your schema is like this:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=angle"},"schema lang.1 {\n\npredicate Declaration :\n    {\n         name : string,\n         source : src.Range,\n     }\n}\n")),(0,i.mdx)("p",null,"now suppose we want to add documentation to the declarations that we indexed. We define a new version of the schema, ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.2"),", with a new ",(0,i.mdx)("inlineCode",{parentName:"p"},"Declaration")," predicate:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=angle"},"schema lang.2 : lang.1 {\n\npredicate Declaration :\n    {\n        name : string,\n        source : src.Range,\n        doc : string\n    }\n}\n")),(0,i.mdx)("p",null,"Now, we proceed to make our changes:"),(0,i.mdx)("ol",null,(0,i.mdx)("li",{parentName:"ol"},"Update the schema"),(0,i.mdx)("li",{parentName:"ol"},"Modify the indexer to generate facts of the new predicate ",(0,i.mdx)("inlineCode",{parentName:"li"},"lang.Declaration.2"))),(0,i.mdx)("p",null,"At this point, any DBs generated by the new indexer will have ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.Declaration.2")," facts, and not ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.Declaration.1"),". Existing clients that query for the old facts will get no results. We can probably recompile those clients to pick up the new ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.Declaration.2")," facts, but that would be a tricky migration: the new client won't work on the old DBs, and the old client won't work on the new DBs."),(0,i.mdx)("p",null,"To make this migration smoother, we can add a derived predicate:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=angle"},"schema lang.2 : lang.1 {\n\npredicate Declaration :\n    {\n        name : string,\n        source : src.Range,\n        doc : string\n    }\n\n derive lang.Declaration.1\n    { name = N, source = S } where\n        lang.Declaration.2 { name = N, source = S }\n}\n")),(0,i.mdx)("p",null,"the ",(0,i.mdx)("inlineCode",{parentName:"p"},"derive lang.Declaration.1")," declaration is just like adding an on-demand derived predicate to ",(0,i.mdx)("inlineCode",{parentName:"p"},"predicate Declaration")," in the ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.1")," schema, but we have to declare it as part of the ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.2")," schema because it needs to refer to ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.Declaration.2"),"."),(0,i.mdx)("p",null,"This derived predicate takes effect as follows:"),(0,i.mdx)("ul",null,(0,i.mdx)("li",{parentName:"ul"},"It ",(0,i.mdx)("strong",{parentName:"li"},"does not apply to old DBs")," that contain ",(0,i.mdx)("inlineCode",{parentName:"li"},"lang.Declaration.1")," but not ",(0,i.mdx)("inlineCode",{parentName:"li"},"lang.Declaration.2"),"."),(0,i.mdx)("li",{parentName:"ul"},"It ",(0,i.mdx)("strong",{parentName:"li"},"does apply to new DBs")," created with the new ",(0,i.mdx)("inlineCode",{parentName:"li"},"lang.Declaration.2")," schema. So after the schema change, we can only create facts of ",(0,i.mdx)("inlineCode",{parentName:"li"},"lang.Declaration.2"),", not the old predicate.")),(0,i.mdx)("p",null,"So clients that query for ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.Declaration.1")," will continue to work both with old DBs containing ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.Declaration.1")," ",(0,i.mdx)("em",{parentName:"p"},"and")," new DBs that contain ",(0,i.mdx)("inlineCode",{parentName:"p"},"lang.Declaration.2"),", and we can migrate them to use the new schema at our leisure."),(0,i.mdx)("h3",{id:"default-derived-predicates"},"Default derived predicates"),(0,i.mdx)("p",null,"There's one extra feature that can be used to make the schema migration even smoother."),(0,i.mdx)("p",null,"Recall with the ",(0,i.mdx)("inlineCode",{parentName:"p"},"derive")," declaration in the previous section we had to synchronise the update of the schema with the rollout of the new version of the indexer to generate the new facts? It's possible to decouple those by making one tweak:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=angle"}," derive lang.Declaration.1 default\n    # ... same as before\n")),(0,i.mdx)("p",null,"The addition of the ",(0,i.mdx)("inlineCode",{parentName:"p"},"default")," keyword to the declaration has the following effect:"),(0,i.mdx)("ul",null,(0,i.mdx)("li",{parentName:"ul"},"A ",(0,i.mdx)("inlineCode",{parentName:"li"},"default")," derived predicate only takes effect when the DB is complete (i.e. read-only) and ",(0,i.mdx)("strong",{parentName:"li"},"contains no facts")," of the predicate.")),(0,i.mdx)("p",null,"This allows us to update the schema but still generate facts of the old predicate. The derived predicate will only kick in when we update the indexer to generate the new facts."),(0,i.mdx)("p",null,"What's more, we can use this technique to provide ",(0,i.mdx)("strong",{parentName:"p"},"forwards compatibility")," too:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=angle"},' derive lang.Declaration.2 default\n    { name = N, source = S, doc = "" } where\n        lang.Declaration.1 { name = N, source = S }\n')),(0,i.mdx)("p",null,"Since this is a ",(0,i.mdx)("inlineCode",{parentName:"p"},"default")," derivation, it will take effect when there are no facts of the new predicate. So we can update clients to work with the new version of the predicate, and they will continue to work on old DBs - albeit with empty strings for the new ",(0,i.mdx)("inlineCode",{parentName:"p"},"doc")," field, because the old DBs don't contain that data."),(0,i.mdx)("h2",{id:"how-do-i-write-and-test-a-derived-predicate"},"How do I write and test a derived predicate?"),(0,i.mdx)("p",null,"There's a process for iterating and testing derived predicates using the shell with a local database. Follow these steps:"),(0,i.mdx)(o.FbInternalOnly,{mdxType:"FbInternalOnly"},(0,i.mdx)("p",null,"Make a dir to store your local Glean DBs."),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=shell"},"mkdir ~/local/gleandb\n")),(0,i.mdx)("p",null,"Download the DB you want to test with:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=shell"},"glean --db-root ~/local/gleandb restore --repo-name fbsource --date 2021-04-29\n")),(0,i.mdx)("p",null,"(replace ",(0,i.mdx)("inlineCode",{parentName:"p"},"fbsource")," and the date as appropriate). This may take a while.")),(0,i.mdx)(o.OssOnly,{mdxType:"OssOnly"},(0,i.mdx)("p",null,"Obtain the DB you want to test with, let's assume you put it in\n",(0,i.mdx)("inlineCode",{parentName:"p"},"~/local/gleandb"),".")),(0,i.mdx)("p",null,"Start the shell with the local DB and schema:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=shell"},"glean-shell --db-root ~/local/gleandb --schema glean/schema/source\n")),(0,i.mdx)("p",null,"Add ",(0,i.mdx)("inlineCode",{parentName:"p"},"--db-schema-override")," if you are working on an existing predicate and want your version to override the schema in the DB."),(0,i.mdx)("p",null,"Select your DB with the ",(0,i.mdx)("inlineCode",{parentName:"p"},":db")," command."),(0,i.mdx)("p",null,"Make edits to the local schema source files in ",(0,i.mdx)("inlineCode",{parentName:"p"},"glean/schema/source"),". There's no need to run ",(0,i.mdx)("inlineCode",{parentName:"p"},"glean/schema/sync"),", you can pick up the changes immediately in the shell:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre",className:"language-lang=shell"},":reload\n")),(0,i.mdx)("p",null,"Test your derived predicate using queries in the shell, use ",(0,i.mdx)("inlineCode",{parentName:"p"},":reload")," to pick up new changes, and repeat as necessary."),(0,i.mdx)("p",null,"The ",(0,i.mdx)("inlineCode",{parentName:"p"},":timeout")," command can be used to change the default query timeout while iterating."),(0,i.mdx)("p",null,"If you run into performance issues, try the techniques in ",(0,i.mdx)("a",{parentName:"p",href:"angle/debugging"},"Debugging\nQueries"),"."),(0,i.mdx)("p",null,"When you're done, the next section describes how to get your derived predicate into the schema proper."),(0,i.mdx)("h2",{id:"how-do-i-make-a-derived-predicate-available"},"How do I make a derived predicate available?"),(0,i.mdx)("p",null,"Derived predicates are defined directly in the schema, so the process\nfor adding them is exactly the same as modifying the schema, described\nover in ",(0,i.mdx)("a",{parentName:"p",href:"/docs/schema/workflow"},"Schema Workflow"),"."))}u.isMDXComponent=!0},47596:function(e,t,n){var a=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,i){function o(e){try{l(a.next(e))}catch(t){i(t)}}function d(e){try{l(a.throw(e))}catch(t){i(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(o,d)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.getSpecInfo=void 0;const r=n(11737);t.getSpecInfo=function(e){return a(this,void 0,void 0,(function*(){return yield r.call({module:"bloks",api:"getSpecInfo",args:{styleId:e}})}))}},11737:function(e,t){var n=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,i){function o(e){try{l(a.next(e))}catch(t){i(t)}}function d(e){try{l(a.throw(e))}catch(t){i(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(o,d)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.call=void 0;let a=!1,r=0;const i={};t.call=function(e){return n(this,void 0,void 0,(function*(){if("staticdocs.thefacebook.com"!==window.location.hostname&&"localhost"!==window.location.hostname)return Promise.reject(new Error("Not running on static docs"));a||(a=!0,window.addEventListener("message",(e=>{if("static-docs-bridge-response"!==e.data.event)return;const t=e.data.id;t in i||console.error(`Recieved response for id: ${t} with no matching receiver`),"response"in e.data?i[t].resolve(e.data.response):i[t].reject(new Error(e.data.error)),delete i[t]})));const t=r++,n=new Promise(((e,n)=>{i[t]={resolve:e,reject:n}})),o={event:"static-docs-bridge-call",id:t,module:e.module,api:e.api,args:e.args},d="localhost"===window.location.hostname?"*":"https://www.internalfb.com";return window.parent.postMessage(o,d),n}))}},24855:function(e,t,n){var a=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,i){function o(e){try{l(a.next(e))}catch(t){i(t)}}function d(e){try{l(a.throw(e))}catch(t){i(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(o,d)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.reportFeatureUsage=t.reportContentCopied=void 0;const r=n(11737);t.reportContentCopied=function(e){return a(this,void 0,void 0,(function*(){const{textContent:t}=e;try{yield r.call({module:"feedback",api:"reportContentCopied",args:{textContent:t}})}catch(n){}}))},t.reportFeatureUsage=function(e){return a(this,void 0,void 0,(function*(){const{featureName:t,id:n}=e;console.log("used feature");try{yield r.call({module:"feedback",api:"reportFeatureUsage",args:{featureName:t,id:n}})}catch(a){}}))}},44256:function(e,t,n){var a=this&&this.__createBinding||(Object.create?function(e,t,n,a){void 0===a&&(a=n),Object.defineProperty(e,a,{enumerable:!0,get:function(){return t[n]}})}:function(e,t,n,a){void 0===a&&(a=n),e[a]=t[n]}),r=this&&this.__setModuleDefault||(Object.create?function(e,t){Object.defineProperty(e,"default",{enumerable:!0,value:t})}:function(e,t){e.default=t}),i=this&&this.__importStar||function(e){if(e&&e.__esModule)return e;var t={};if(null!=e)for(var n in e)"default"!==n&&Object.prototype.hasOwnProperty.call(e,n)&&a(t,e,n);return r(t,e),t};Object.defineProperty(t,"__esModule",{value:!0}),t.OssOnly=t.FbInternalOnly=t.isInternal=t.validateFbContentArgs=t.fbInternalOnly=t.fbContent=t.inpageeditor=t.feedback=t.uidocs=t.bloks=void 0,t.bloks=i(n(47596)),t.uidocs=i(n(17483)),t.feedback=i(n(24855)),t.inpageeditor=i(n(27312));const o=["internal","external"];function d(e){return c(e),s()?"internal"in e?l(e.internal):[]:"external"in e?l(e.external):[]}function l(e){return"function"==typeof e?e():e}function c(e){if("object"!=typeof e)throw new Error(`fbContent() args must be an object containing keys: ${o}. Instead got ${e}`);if(!Object.keys(e).find((e=>o.find((t=>t===e)))))throw new Error(`No valid args found in ${JSON.stringify(e)}. Accepted keys: ${o}`);const t=Object.keys(e).filter((e=>!o.find((t=>t===e))));if(t.length>0)throw new Error(`Unexpected keys ${t} found in fbContent() args. Accepted keys: ${o}`)}function s(){try{return Boolean(!1)}catch(e){return console.log("process.env.FB_INTERNAL couldn't be read, maybe you forgot to add the required webpack EnvironmentPlugin config?",e),!1}}t.fbContent=d,t.fbInternalOnly=function(e){return d({internal:e})},t.validateFbContentArgs=c,t.isInternal=s,t.FbInternalOnly=function(e){return s()?e.children:null},t.OssOnly=function(e){return s()?null:e.children}},27312:function(e,t,n){var a=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,i){function o(e){try{l(a.next(e))}catch(t){i(t)}}function d(e){try{l(a.throw(e))}catch(t){i(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(o,d)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.submitDiff=void 0;const r=n(11737);t.submitDiff=function(e){return a(this,void 0,void 0,(function*(){const{file_path:t,new_content:n,project_name:a}=e;try{return yield r.call({module:"inpageeditor",api:"createPhabricatorDiffApi",args:{file_path:t,new_content:n,project_name:a}})}catch(i){throw new Error(`Error occurred while trying to submit diff. Stack trace: ${i}`)}}))}},17483:function(e,t,n){var a=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,i){function o(e){try{l(a.next(e))}catch(t){i(t)}}function d(e){try{l(a.throw(e))}catch(t){i(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(o,d)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.getApi=t.docsets=void 0;const r=n(11737);t.docsets={BLOKS_CORE:"887372105406659"},t.getApi=function(e){return a(this,void 0,void 0,(function*(){const{name:t,framework:n,docset:a}=e;return yield r.call({module:"uidocs",api:"getApi",args:{name:t,framework:n,docset:a}})}))}}}]);