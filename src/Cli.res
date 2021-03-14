// START: SvelteKit Builder
// Builder definition - @sveltejs/kit/dist/index6.js#L674
type prerenderOptions = {
  force: bool,
  dest: string,
}
type builder = {
  copy_client_files: (. string) => unit,
  copy_server_files: (. string) => unit,
  copy_static_files: (. string) => unit,
  prerender: (. prerenderOptions) => Js.Promise.t<unit>,
  // log.warn etc??
}
// END: SvelteKit Builder

// START: JS FFIs
@module("fs")
external existsSync: (~path: string) => bool = "existsSync"

@module("fs")
external readFileSync: (
  ~path: string,
  ~options: [#ascii | #utf8 | #utf16le | #ucs2 | #base64 | #latin1 | #binary | #hex],
) => string = "readFileSync"

@module("path") @variadic
external join: (~paths: array<string>) => string = "join"

@module("path")
external dirname: string => string = "dirname"

@module("@sveltejs/app-utils/files")
external svelteKitCopy: (~from: string, ~toDest: string, ~filter: unit => bool=?, unit) => unit =
  "copy" //array<string> = "copy"
// END: JS FFIs

let getFile = filepath => {
  if existsSync(~path=filepath) {
    readFileSync(~path=filepath, ~options=#utf8)
  } else {
    Js.Exn.raiseError(`File ${filepath} does not exist.`)
  }
}

// I do not know if there is a better way to do this. Perhaps the initial parse of the Firebase configuration should be like the bound rule below
type siteConfig = {public: string}
type cloudRunRewrite = {serviceId: string}
type rewriteConfig = {
  source: string,
  run: option<cloudRunRewrite>,
  function: option<string>,
}
@scope("JSON") @val
external parseFirebaseRewriteRule: string => rewriteConfig = "parse"

let getSiteConfiguration = () => {
  // TODO: fix this
  {public: "hello"}
}
let getRewriteConfiguration = (
  firebaseJsonFilePath,
  hostingSite,
  sourceRewriteMatch,
): rewriteConfig => {
  let json = try Js.Json.parseExn(getFile(firebaseJsonFilePath)) catch {
  | _ =>
    Js.Exn.raiseError(`Error parsing Firebase Configuration ${firebaseJsonFilePath}. Invalid JSON.`)
  }
  switch Js.Json.classify(json) {
  | Js.Json.JSONObject(value) =>
    switch Js.Dict.get(value, "hosting") {
    | Some(hostingConfig) =>
      let hosting = switch Js.Json.decodeArray(hostingConfig) {
      | Some(hostingConfig) => hostingConfig // returns "hosting[*]" that matches hostingSite
      | None =>
        // force the universally applicable "array" syntax over "object"
        // TODO attempt to perform decodeObject and error if not an object, and return single element array here
        Js.Exn.raiseError(
          `${firebaseJsonFilePath} error. Expected "hosting" property to be "array". Try wrapping your "hosting" object in an array [].`,
        )
      }

      let matchedSite = switch hostingSite {
      // if no hostingSite provided, assume single item array
      | None =>
        switch Js.Array2.shift(hosting) {
        | None =>
          Js.Exn.raiseError(
            `${firebaseJsonFilePath} error. "hosting.[]" is an empty array. At least one hosting site config required.`,
          )
        | Some(hostingSite) => hostingSite
        }
      // iterate to find a match
      | Some(site) =>
        switch Js.Array2.shift(
          Js.Array2.filteri(hosting, (item, index) => {
            switch Js.Json.decodeObject(item) {
            | None =>
              Js.Exn.raiseError(
                `${firebaseJsonFilePath} error. Hosting item was not a valid JSON Object.`,
              )
            | Some(i) =>
              switch Js.Dict.get(i, "site") {
              | None =>
                Js.Exn.raiseError(
                  `${firebaseJsonFilePath} error. Hosting item as position ${Belt.Int.toString(
                      index,
                    )} does not have required "site" field.`,
                )
              | Some(siteConfig) => siteConfig === site
              }
            }
          }),
        ) {
        | None =>
          Js.Exn.raiseError(
            `${firebaseJsonFilePath} error. No "hosting[].site" match for ${Js.Json.stringify(
                site,
              )}.`,
          )
        | Some(hostingSite) => hostingSite
        }
      }

      switch Js.Json.decodeObject(matchedSite) {
      | None =>
        Js.Exn.raiseError(
          `${firebaseJsonFilePath} error. Hosting match is not a valid JSON object.`,
        )
      | Some(siteConfig) =>
        switch Js.Dict.get(siteConfig, "rewrites") {
        | None =>
          Js.Exn.raiseError(
            `${firebaseJsonFilePath} error. Hosting config does not have required "rewrites":[] field.`,
          )
        | Some(rewriteRules) =>
          switch Js.Json.decodeArray(rewriteRules) {
          | None =>
            Js.Exn.raiseError(
              `${firebaseJsonFilePath} error. Hosting config "rewrites":[] field is not an array.`,
            )
          | Some(rules) =>
            switch Js.Array2.findi(rules, (item, index) => {
              switch Js.Json.decodeObject(item) {
              | None =>
                Js.Exn.raiseError(
                  `${firebaseJsonFilePath} error. "rewrites" item at position ${Belt.Int.toString(
                      index,
                    )} is not an object`,
                )
              | Some(rule) =>
                switch Js.Dict.get(rule, "source") {
                | None =>
                  Js.Exn.raiseError(
                    `${firebaseJsonFilePath} error. Missing "source" field in rewrite rules.`,
                  )
                | Some(source) =>
                  sourceRewriteMatch === source &&
                    switch Js.Dict.get(rule, "run") {
                    | None =>
                      switch Js.Dict.get(rule, "function") {
                      | None => false
                      | Some(func) => true
                      }
                    | Some(run) => true
                    }
                }
              }
            }) {
            | None =>
              Js.Exn.raiseError(
                `${firebaseJsonFilePath} error. No rewrites item matching "source":"${Js.Json.stringify(
                    sourceRewriteMatch,
                  )}". The rule must contain either "function":"" or "run":{} configuration as well.`,
              )
            | Some(rule) => parseFirebaseRewriteRule(Js.Json.stringify(rule))
            }
          }
        }
      }
    | None => Js.Exn.raiseError(`${firebaseJsonFilePath} error. Expected "hosting" property.`)
    }
  | _ =>
    Js.Exn.raiseError(`${firebaseJsonFilePath} error. Expected root level JSON to be an object.`)
  }
}

let adapt = (~builder: builder, ~adapterConfig=Js.Obj.empty(), ()) => {
  let config = Js.Obj.assign(
    {
      "cloudRunBuildDir": None,
      "firebaseJson": "firebase.json",
      "hostingSite": None,
      "sourceRewriteMatch": "**",
    },
    adapterConfig,
  )

  let siteConfig: siteConfig = getSiteConfiguration()
  let rewriteConfiguration = getRewriteConfiguration(
    config["firebaseJson"],
    config["hostingSite"],
    config["sourceRewriteMatch"],
  )

  switch rewriteConfiguration.run {
  | Some(run) =>
    Js.log2("Cloud Run configuration", run)
    let serverOutputDir = switch config["cloudRunBuildDir"] {
    | None => join(~paths=[run.serviceId])
    | Some(dir) => join(~paths=[dir])
    }

    // TODO: use builder.log.warn instead
    Js.log(`Writing Cloud Run service to ./${serverOutputDir}`)
    builder.copy_server_files(. serverOutputDir)

    // TODO: improve __dirname usage
    let dirname = switch %external(__dirname) {
    | Some(dirname) => dirname
    | None => Js.Exn.raiseError("FATAL")
    }
    svelteKitCopy(~from=join(~paths=[dirname, "./files"]), ~toDest=serverOutputDir, ())
    // TODO: use builder.log.warn instead
    Js.log(
      `To deploy your Cloud Run service, run this command:
+--------------------------------------------------+
gcloud beta run --platform managed --region us-central1 deploy ${run.serviceId} --source ${serverOutputDir} --allow-unauthenticated
+--------------------------------------------------+`,
    )

  | None => // TODO: Cloud Function here, if no Cloud Function or Cloud Run hard-error
    ()
  }

  let staticOutputDir = join(~paths=[siteConfig.public])
  // TODO: use builder.log.warn instead
  Js.log(`Writing client application to ${staticOutputDir}`)
  builder.copy_static_files(. staticOutputDir)
  builder.copy_client_files(. staticOutputDir)
  // TODO: use builder.log.warn instead
  Js.log(`Prerendering static pages to ${staticOutputDir}`)
  builder.prerender(. {force: false, dest: staticOutputDir})
}

// adapter function needs to be the default
// TODO: update sig to new adapter API
let default = adapt

// 	// @ this point Joi has validated that all required values are in the config file
// 	const firebaseSiteConfig =
// 		firebaseConfig.hosting.length === 1 ?
// 			firebaseConfig.hosting[0] : // Site field is optional for single site configs, so just grab first
// 			firebaseConfig.hosting.find(item => item.site === hostingSite);
// 	const firebaseRewriteConfig = firebaseSiteConfig.rewrites.find(item => {
// 		return item.source === sourceRewriteMatch && (item.function || item.run);
// 	});
