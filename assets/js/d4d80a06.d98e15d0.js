"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[2345],{3905:function(e,t,n){n.r(t),n.d(t,{MDXContext:function(){return d},MDXProvider:function(){return u},mdx:function(){return x},useMDXComponents:function(){return s},withMDXComponents:function(){return p}});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(){return(o=Object.assign||function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var a in n)Object.prototype.hasOwnProperty.call(n,a)&&(e[a]=n[a])}return e}).apply(this,arguments)}function m(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?m(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):m(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var d=a.createContext({}),p=function(e){return function(t){var n=s(t.components);return a.createElement(e,o({},t,{components:n}))}},s=function(e){var t=a.useContext(d),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},u=function(e){var t=s(e.components);return a.createElement(d.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},f=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,m=e.parentName,d=l(e,["components","mdxType","originalType","parentName"]),p=s(n),u=r,f=p["".concat(m,".").concat(u)]||p[u]||c[u]||o;return n?a.createElement(f,i(i({ref:t},d),{},{components:n})):a.createElement(f,i({ref:t},d))}));function x(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,m=new Array(o);m[0]=f;var i={};for(var l in t)hasOwnProperty.call(t,l)&&(i[l]=t[l]);i.originalType=e,i.mdxType="string"==typeof e?e:r,m[1]=i;for(var d=2;d<o;d++)m[d]=n[d];return a.createElement.apply(null,m)}return a.createElement.apply(null,n)}f.displayName="MDXCreateElement"},40902:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return i},contentTitle:function(){return l},metadata:function(){return d},toc:function(){return p},default:function(){return u}});var a=n(87462),r=n(63366),o=(n(67294),n(3905)),m=(n(44256),["components"]),i={id:"reference",title:"Angle Reference",sidebar_label:"Reference"},l=void 0,d={unversionedId:"angle/reference",id:"angle/reference",isDocsHomePage:!1,title:"Angle Reference",description:"Queries",source:"@site/../docs/angle/reference.md",sourceDirName:"angle",slug:"/angle/reference",permalink:"/docs/angle/reference",editUrl:"https://www.internalfb.com/intern/diffusion/FBS/browse/master/fbcode/glean/website/../docs/angle/reference.md",version:"current",frontMatter:{id:"reference",title:"Angle Reference",sidebar_label:"Reference"},sidebar:"someSidebar",previous:{title:"Debugging",permalink:"/docs/angle/debugging"},next:{title:"Style Guide",permalink:"/docs/angle/style"}},p=[{value:"Queries",id:"queries",children:[]},{value:"Statements",id:"statements",children:[]},{value:"Names",id:"names",children:[]},{value:"Term",id:"term",children:[]},{value:"Primitives",id:"primitives",children:[]}],s={toc:p};function u(e){var t=e.components,n=(0,r.Z)(e,m);return(0,o.mdx)("wrapper",(0,a.Z)({},s,n,{components:t,mdxType:"MDXLayout"}),(0,o.mdx)("h2",{id:"queries"},"Queries"),(0,o.mdx)("p",null,"A query produces a set of values. At the outermost level, the values\nreturned are always ",(0,o.mdx)("em",{parentName:"p"},"facts"),", which are returned to the client making\nthe query."),(0,o.mdx)("p",null,"In general, a Glean query takes the form:"),(0,o.mdx)("p",null,(0,o.mdx)("em",{parentName:"p"},"query")," ::= ","[ ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"where")," ]"," ",(0,o.mdx)("em",{parentName:"p"},"statement\u2080")," ; ...; ",(0,o.mdx)("em",{parentName:"p"},"statement\u2099")),(0,o.mdx)("p",null,"You can think of this declaratively, as in"),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"For each substitution of the variables in the query such that ",(0,o.mdx)("em",{parentName:"p"},"statement\u2080"),"..",(0,o.mdx)("em",{parentName:"p"},"statement\u2099")," holds, produce the value of ",(0,o.mdx)("em",{parentName:"p"},"term"))),(0,o.mdx)("p",null,"Or, we can think of it more operationally, which helps with query optimisation:"),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"for each value of ",(0,o.mdx)("em",{parentName:"p"},"statement\u2080"),(0,o.mdx)("br",null),"\n...",(0,o.mdx)("br",null),"\nfor each value of ",(0,o.mdx)("em",{parentName:"p"},"statement\u2099"),(0,o.mdx)("br",null),"\nproduce the value of ",(0,o.mdx)("em",{parentName:"p"},"term"))),(0,o.mdx)("p",null,"If ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"where")," is omitted, then the query produces the values of the final statement. For example, a query ",(0,o.mdx)("inlineCode",{parentName:"p"},'src.File "foo/bar"')," is equivalent to ",(0,o.mdx)("inlineCode",{parentName:"p"},'F where F = src.File "foo/bar"'),"."),(0,o.mdx)("p",null,"Note that a query corresponds to a nested loop, where ",(0,o.mdx)("em",{parentName:"p"},"statement\u2080")," is the outermost loop, and ",(0,o.mdx)("em",{parentName:"p"},"statement\u2099")," is the innermost. The ordering of the statements can therefore have a significant effect on performance."),(0,o.mdx)("h2",{id:"statements"},"Statements"),(0,o.mdx)("p",null,(0,o.mdx)("em",{parentName:"p"},"statement")," ::= ","[ ",(0,o.mdx)("em",{parentName:"p"},"term\u2081")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"=")," ]"," ",(0,o.mdx)("em",{parentName:"p"},"term\u2082")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"match all values of ",(0,o.mdx)("strong",{parentName:"p"},"term\u2081")," against all values of ",(0,o.mdx)("strong",{parentName:"p"},"term\u2082"))),(0,o.mdx)("p",null,"The order is mostly irrelevant; ",(0,o.mdx)("inlineCode",{parentName:"p"},"A = B")," is equivalent to ",(0,o.mdx)("inlineCode",{parentName:"p"},"B = A"),", except that type inference works by inferring the right-hand-side before checking the left-hand-side so this may influence which order you want. You can also use a type signature (",(0,o.mdx)("inlineCode",{parentName:"p"},"A = B : type"),") to help the type checker."),(0,o.mdx)("h2",{id:"names"},"Names"),(0,o.mdx)("p",null,"Glean uses the following classes of names:"),(0,o.mdx)("ul",null,(0,o.mdx)("li",{parentName:"ul"},"A ",(0,o.mdx)("em",{parentName:"li"},"schema name"),", e.g. ",(0,o.mdx)("inlineCode",{parentName:"li"},"search.cxx"),", of the form ",(0,o.mdx)("em",{parentName:"li"},"name"),"[.",(0,o.mdx)("em",{parentName:"li"},"name"),"]","*. By convention, the components of a schema name begin with a lower-case letter."),(0,o.mdx)("li",{parentName:"ul"},"A ",(0,o.mdx)("em",{parentName:"li"},"predicate name"),", e.g. ",(0,o.mdx)("inlineCode",{parentName:"li"},"cxx.Name.1")," of the form ",(0,o.mdx)("em",{parentName:"li"},"schema"),".",(0,o.mdx)("em",{parentName:"li"},"predicate"),"[.",(0,o.mdx)("em",{parentName:"li"},"version"),"]",". By convention, ",(0,o.mdx)("em",{parentName:"li"},"predicate")," begins with an upper-case letter. The version can often be omitted, in which case it defaults depending on the context: in a query it defaults to the most recent version, in a schema there is always only one version of a predicate visible in any given scope."),(0,o.mdx)("li",{parentName:"ul"},"A ",(0,o.mdx)("em",{parentName:"li"},"field name"),", e.g. ",(0,o.mdx)("inlineCode",{parentName:"li"},"declaration"),", used to identify fields of a record, or alternatives of a sum type or enumeration.  A field name ",(0,o.mdx)("strong",{parentName:"li"},"must begin with a lower-case letter"),"."),(0,o.mdx)("li",{parentName:"ul"},"A ",(0,o.mdx)("em",{parentName:"li"},"variable"),", e.g. ",(0,o.mdx)("inlineCode",{parentName:"li"},"X"),". Variables ",(0,o.mdx)("strong",{parentName:"li"},"must begin with an upper-case letter")," to distinguish them from field names.")),(0,o.mdx)("p",null,"There is a set of reserved words that can't be used for names. Mostly this is because those words would clash with reserved keywords in code that we generate from the schema, and we don't want to have to do any automatic translation of names that might be confusing. Typically the convention for avoiding these reserved words is to add an underscore to the name, e.g. ",(0,o.mdx)("inlineCode",{parentName:"p"},"class_"),"."),(0,o.mdx)("h2",{id:"term"},"Term"),(0,o.mdx)("p",null,"A term may be fully defined, like ",(0,o.mdx)("inlineCode",{parentName:"p"},"{ true, 123 }")," (a value that we could insert in the database), or it can be partially defined, like ",(0,o.mdx)("inlineCode",{parentName:"p"},'{ A, "b", _ }'),"."),(0,o.mdx)("p",null,"A term is often matched against something that will instantiate its unknown variables. For example, in ",(0,o.mdx)("inlineCode",{parentName:"p"},"cxx.Name X"),", we're instantitating the variable ",(0,o.mdx)("inlineCode",{parentName:"p"},"X")," to each of the keys of the predicate ",(0,o.mdx)("inlineCode",{parentName:"p"},"cxx.Name"),"."),(0,o.mdx)("p",null,"Ultimately the result of a query must be terms that are fully defined, though. If this isn't the case, Glean's query engine will report an error.  For example, a query like ",(0,o.mdx)("inlineCode",{parentName:"p"},"X where 123")," doesn't make sense, because we haven't matched ",(0,o.mdx)("inlineCode",{parentName:"p"},"X")," with anything."),(0,o.mdx)("p",null,"Terms have the following forms:"),(0,o.mdx)("p",null,(0,o.mdx)("em",{parentName:"p"},"term")," ::=",(0,o.mdx)("br",null),"\n","\xa0","\xa0","    ",(0,o.mdx)("em",{parentName:"p"},"variable")," ",(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"A ",(0,o.mdx)("strong",{parentName:"p"},"variable")," names the terms that match at this position in the query. The variable can be menioned elsewhere in the query; it doesn't usually make sense for a variable to be mentioned only once, since then you might as well just use a wildcard, see below.")),(0,o.mdx)("p",null,"\xa0","\xa0","  ",(0,o.mdx)("inlineCode",{parentName:"p"},"_"),(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"A wildcard; matches anything")),(0,o.mdx)("p",null,"\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"predicate"),"\xa0",(0,o.mdx)("em",{parentName:"p"},"term")," ","[ ",(0,o.mdx)("inlineCode",{parentName:"p"},"->")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ]"," ",(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"All the facts of ",(0,o.mdx)("strong",{parentName:"p"},"predicate")," with keys that match the first ",(0,o.mdx)("strong",{parentName:"p"},"term")," (and values that match the second ",(0,o.mdx)("strong",{parentName:"p"},"term")," if appropriate)")),(0,o.mdx)("p",null,"\xa0","\xa0",(0,o.mdx)("inlineCode",{parentName:"p"},"(")," ",(0,o.mdx)("em",{parentName:"p"},"query")," ",(0,o.mdx)("inlineCode",{parentName:"p"},")")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"All the values of ",(0,o.mdx)("strong",{parentName:"p"},"query"),". Note in particular that ",(0,o.mdx)("strong",{parentName:"p"},"query")," can just be a simple term, but it can also be something like ",(0,o.mdx)("strong",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"where")," ",(0,o.mdx)("strong",{parentName:"p"},"statements"),".")),(0,o.mdx)("p",null,"\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"[..]")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"All the elements of the array ",(0,o.mdx)("strong",{parentName:"p"},"term"))),(0,o.mdx)("p",null,"\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"term\u2081")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"|")," ",(0,o.mdx)("em",{parentName:"p"},"term\u2082")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"When used as a pattern, matches ",(0,o.mdx)("strong",{parentName:"p"},"term\u2081")," or ",(0,o.mdx)("strong",{parentName:"p"},"term\u2082"),". When used as an expression, generates all values of ",(0,o.mdx)("strong",{parentName:"p"},"term\u2081")," and all values of ",(0,o.mdx)("strong",{parentName:"p"},"term\u2082"),".")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"Note: variables mentioned in ",(0,o.mdx)("strong",{parentName:"p"},"term\u2081")," and ",(0,o.mdx)("strong",{parentName:"p"},"term\u2082")," are local to those terms, and may have different types, but only if the variable is not mentioned elsewhere.")),(0,o.mdx)("p",null,"\xa0","\xa0","  ",(0,o.mdx)("em",{parentName:"p"},"[0-9]","+"),(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"a number matches a value of type ",(0,o.mdx)("inlineCode",{parentName:"p"},"nat")," or ",(0,o.mdx)("inlineCode",{parentName:"p"},"byte"))),(0,o.mdx)("p",null,"\xa0","\xa0","  ",(0,o.mdx)("em",{parentName:"p"},"string"),(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"a string matches a value of type ",(0,o.mdx)("inlineCode",{parentName:"p"},"string"))),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("em",{parentName:"p"},"string")," ",(0,o.mdx)("inlineCode",{parentName:"p"},".."),(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"matches strings with the given prefix")),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("em",{parentName:"p"},"string")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"..")," ",(0,o.mdx)("em",{parentName:"p"},"term"),(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"matches both a prefix and a suffix of a string")),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("inlineCode",{parentName:"p"},"{")," ",(0,o.mdx)("em",{parentName:"p"},"field")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"=")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},",")," ... ",(0,o.mdx)("inlineCode",{parentName:"p"},"}"),(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"matches a record with the given fields")),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("inlineCode",{parentName:"p"},"{")," ",(0,o.mdx)("em",{parentName:"p"},"field")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"=")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"}")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"matches a sum type with an alternative ",(0,o.mdx)("strong",{parentName:"p"},"field"))),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("em",{parentName:"p"},"field")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"when matching a sum type, shorthand for ",(0,o.mdx)("inlineCode",{parentName:"p"},"{")," ",(0,o.mdx)("em",{parentName:"p"},"field")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"= _ }"))),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("em",{parentName:"p"},"enumerator")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"matches an value of an enumerated type")),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("inlineCode",{parentName:"p"},"{ just =")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"}"),(0,o.mdx)("br",null),"\n","\xa0","\xa0"," ",(0,o.mdx)("inlineCode",{parentName:"p"},"nothing")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"matches a ",(0,o.mdx)("inlineCode",{parentName:"p"},"maybe")," type")),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("inlineCode",{parentName:"p"},"true"),(0,o.mdx)("br",null),"\n","\xa0","\xa0"," ",(0,o.mdx)("inlineCode",{parentName:"p"},"false")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"matches a ",(0,o.mdx)("inlineCode",{parentName:"p"},"boolean"))),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("em",{parentName:"p"},"term"),"\xa0",":","\xa0",(0,o.mdx)("em",{parentName:"p"},"type"),(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"(a ",(0,o.mdx)("em",{parentName:"p"},"type signature"),") interpret ",(0,o.mdx)("strong",{parentName:"p"},"term")," as having type ",(0,o.mdx)("strong",{parentName:"p"},"type"),", where ",(0,o.mdx)("strong",{parentName:"p"},"type")," is any valid Angle type.")),(0,o.mdx)("p",null,"\xa0","\xa0"," ",(0,o.mdx)("inlineCode",{parentName:"p"},"$")," ","[0-9]","+",(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"matches a literal fact ID. The only reason to use these would be if you did a previous query, extracted some fact IDs, and want to do a subsequent query incorporating them. Literal fact IDs are not allowed in derived predicates (it wouldn't make any sense).")),(0,o.mdx)("h2",{id:"primitives"},"Primitives"),(0,o.mdx)("p",null,"Angle supports a few primitive operations. The argument(s) to a primitive operation must always be fully defined; they cannot be patterns or wildcards."),(0,o.mdx)("p",null,"\xa0","\xa0",(0,o.mdx)("inlineCode",{parentName:"p"},"prim.toLower")," (S : string) : string"),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"Converts its string argument to lower case")),(0,o.mdx)("p",null,"\xa0","\xa0",(0,o.mdx)("inlineCode",{parentName:"p"},"prim.length")," (A : ","[_]",") : nat"),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"Equal to the number of elements in its array argument")),(0,o.mdx)("p",null,"\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},">")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("br",null),"\n","\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},">=")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("br",null),"\n","\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"<")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("br",null),"\n","\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"<=")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("br",null),"\n","\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"!==")," ",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("br",null)),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"Standard numerical comparisons. These work on values of type ",(0,o.mdx)("inlineCode",{parentName:"p"},"nat")," only, and they have value ",(0,o.mdx)("inlineCode",{parentName:"p"},"{}")," if the comparison succeeds, otherwise they fail (in the same way as a predicate match fails if there are no facts that match the pattern).")),(0,o.mdx)("p",null,"\xa0","\xa0",(0,o.mdx)("em",{parentName:"p"},"term")," ",(0,o.mdx)("inlineCode",{parentName:"p"},"!=")," ",(0,o.mdx)("em",{parentName:"p"},"term")),(0,o.mdx)("blockquote",null,(0,o.mdx)("p",{parentName:"blockquote"},"Standard comparison between two terms of any type. It has a value of ",(0,o.mdx)("inlineCode",{parentName:"p"},"{}")," if the comparison succeeds, otherwise it fails in the same way as a predicate match fails if there are no facts that match the pattern.")))}u.isMDXComponent=!0},47596:function(e,t,n){var a=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,o){function m(e){try{l(a.next(e))}catch(t){o(t)}}function i(e){try{l(a.throw(e))}catch(t){o(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(m,i)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.getSpecInfo=void 0;const r=n(11737);t.getSpecInfo=function(e){return a(this,void 0,void 0,(function*(){return yield r.call({module:"bloks",api:"getSpecInfo",args:{styleId:e}})}))}},11737:function(e,t){var n=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,o){function m(e){try{l(a.next(e))}catch(t){o(t)}}function i(e){try{l(a.throw(e))}catch(t){o(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(m,i)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.call=void 0;let a=!1,r=0;const o={};t.call=function(e){return n(this,void 0,void 0,(function*(){if("staticdocs.thefacebook.com"!==window.location.hostname&&"localhost"!==window.location.hostname)return Promise.reject(new Error("Not running on static docs"));a||(a=!0,window.addEventListener("message",(e=>{if("static-docs-bridge-response"!==e.data.event)return;const t=e.data.id;t in o||console.error(`Recieved response for id: ${t} with no matching receiver`),"response"in e.data?o[t].resolve(e.data.response):o[t].reject(new Error(e.data.error)),delete o[t]})));const t=r++,n=new Promise(((e,n)=>{o[t]={resolve:e,reject:n}})),m={event:"static-docs-bridge-call",id:t,module:e.module,api:e.api,args:e.args},i="localhost"===window.location.hostname?"*":"https://www.internalfb.com";return window.parent.postMessage(m,i),n}))}},24855:function(e,t,n){var a=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,o){function m(e){try{l(a.next(e))}catch(t){o(t)}}function i(e){try{l(a.throw(e))}catch(t){o(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(m,i)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.reportFeatureUsage=t.reportContentCopied=void 0;const r=n(11737);t.reportContentCopied=function(e){return a(this,void 0,void 0,(function*(){const{textContent:t}=e;try{yield r.call({module:"feedback",api:"reportContentCopied",args:{textContent:t}})}catch(n){}}))},t.reportFeatureUsage=function(e){return a(this,void 0,void 0,(function*(){const{featureName:t,id:n}=e;console.log("used feature");try{yield r.call({module:"feedback",api:"reportFeatureUsage",args:{featureName:t,id:n}})}catch(a){}}))}},44256:function(e,t,n){var a=this&&this.__createBinding||(Object.create?function(e,t,n,a){void 0===a&&(a=n),Object.defineProperty(e,a,{enumerable:!0,get:function(){return t[n]}})}:function(e,t,n,a){void 0===a&&(a=n),e[a]=t[n]}),r=this&&this.__setModuleDefault||(Object.create?function(e,t){Object.defineProperty(e,"default",{enumerable:!0,value:t})}:function(e,t){e.default=t}),o=this&&this.__importStar||function(e){if(e&&e.__esModule)return e;var t={};if(null!=e)for(var n in e)"default"!==n&&Object.prototype.hasOwnProperty.call(e,n)&&a(t,e,n);return r(t,e),t};Object.defineProperty(t,"__esModule",{value:!0}),t.OssOnly=t.FbInternalOnly=t.isInternal=t.validateFbContentArgs=t.fbInternalOnly=t.fbContent=t.inpageeditor=t.feedback=t.uidocs=t.bloks=void 0,t.bloks=o(n(47596)),t.uidocs=o(n(17483)),t.feedback=o(n(24855)),t.inpageeditor=o(n(27312));const m=["internal","external"];function i(e){return d(e),p()?"internal"in e?l(e.internal):[]:"external"in e?l(e.external):[]}function l(e){return"function"==typeof e?e():e}function d(e){if("object"!=typeof e)throw new Error(`fbContent() args must be an object containing keys: ${m}. Instead got ${e}`);if(!Object.keys(e).find((e=>m.find((t=>t===e)))))throw new Error(`No valid args found in ${JSON.stringify(e)}. Accepted keys: ${m}`);const t=Object.keys(e).filter((e=>!m.find((t=>t===e))));if(t.length>0)throw new Error(`Unexpected keys ${t} found in fbContent() args. Accepted keys: ${m}`)}function p(){try{return Boolean(!1)}catch(e){return console.log("process.env.FB_INTERNAL couldn't be read, maybe you forgot to add the required webpack EnvironmentPlugin config?",e),!1}}t.fbContent=i,t.fbInternalOnly=function(e){return i({internal:e})},t.validateFbContentArgs=d,t.isInternal=p,t.FbInternalOnly=function(e){return p()?e.children:null},t.OssOnly=function(e){return p()?null:e.children}},27312:function(e,t,n){var a=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,o){function m(e){try{l(a.next(e))}catch(t){o(t)}}function i(e){try{l(a.throw(e))}catch(t){o(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(m,i)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.submitDiff=void 0;const r=n(11737);t.submitDiff=function(e){return a(this,void 0,void 0,(function*(){const{file_path:t,new_content:n,project_name:a}=e;try{return yield r.call({module:"inpageeditor",api:"createPhabricatorDiffApi",args:{file_path:t,new_content:n,project_name:a}})}catch(o){throw new Error(`Error occurred while trying to submit diff. Stack trace: ${o}`)}}))}},17483:function(e,t,n){var a=this&&this.__awaiter||function(e,t,n,a){return new(n||(n=Promise))((function(r,o){function m(e){try{l(a.next(e))}catch(t){o(t)}}function i(e){try{l(a.throw(e))}catch(t){o(t)}}function l(e){var t;e.done?r(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(m,i)}l((a=a.apply(e,t||[])).next())}))};Object.defineProperty(t,"__esModule",{value:!0}),t.getApi=t.docsets=void 0;const r=n(11737);t.docsets={BLOKS_CORE:"887372105406659"},t.getApi=function(e){return a(this,void 0,void 0,(function*(){const{name:t,framework:n,docset:a}=e;return yield r.call({module:"uidocs",api:"getApi",args:{name:t,framework:n,docset:a}})}))}}}]);