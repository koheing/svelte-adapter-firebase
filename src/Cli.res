// START: SvelteKit Builder
// Builder definition - @sveltejs/kit/dist/index6.js#L674
type prerenderOptions = {
  force: bool,
  dest: string,
}
type builder = {
  copy_client_files: string => unit,
  copy_server_files: string => unit,
  copy_static_files: string => unit,
  prerender: prerenderOptions => Js.Promise.t<unit>,
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

@module("@sveltejs/app-utils/files")
external copy: (~from: string, ~toDest: string, ~filter: unit => bool=?, unit) => array<string> =
  "copy"
// END: JS FFIs

let getFile = filepath => {
  if existsSync(~path=filepath) {
    readFileSync(~path=filepath, ~options=#utf8)
  } else {
    Js.Exn.raiseError(`File ${filepath} does not exist.`)
  }
}

let getRewriteConfiguration = () => {

}

let adapter = (~builder: builder, ~adapterConfig=Js.Obj.empty(), ()) => {
  let config = Js.Obj.assign(
    {
      "hostingSite": None,
      "sourceRewriteMatch": "**",
      "firebaseJson": "firebase.json",
      "cloudRunBuildDir": None,
    },
    adapterConfig,
  )
  Js.log2("config", config)

  let json = try Js.Json.parseExn(getFile(config["firebaseJson"])) catch {
  | _ =>
    Js.Exn.raiseError(
      `Error parsing Firebase Configuration ${config["firebaseJson"]}. Invalid JSON.`,
    )
  }

  let rewriteConfiguration = switch Js.Json.classify(json) {
  | Js.Json.JSONObject(value) =>
    switch Js.Dict.get(value, "hosting") {
    | Some(hostingConfig) =>
      let hosting = switch Js.Json.decodeArray(hostingConfig) {
      | Some(hostingConfig) => hostingConfig // returns "hosting[*]" that matches hostingSite
      | None =>
        // force the universally applicable "array" syntax over "object"
        // TODO attempt to perform decodeObject and error if not an object, and return single element array here
        Js.Exn.raiseError(
          `${config["firebaseJson"]} error. Expected "hosting" property to be "array". Try wrapping your "hosting" object in an array [].`,
        )
      }

      let matchedSite = switch config["hostingSite"] {
      // if no hostingSite provided, assume single item array
      | None =>
        switch Js.Array.shift(hosting) {
        | None =>
          Js.Exn.raiseError(
            `${config["firebaseJson"]} error. "hosting.[]" is an empty array. At least one hosting site config required.`,
          )
        | Some(hostingSite) => hostingSite
        }
      // iterate to find a match
      | Some(site) =>
        switch Js.Array.shift(Js.Array.filteri((item, index) => {
            switch Js.Json.decodeObject(item) {
            | None =>
              Js.Exn.raiseError(
                `${config["firebaseJson"]} error. Hosting item was not a valid JSON Object.`,
              )
            | Some(i) =>
              switch Js.Dict.get(i, "site") {
              | None =>
                Js.Exn.raiseError(
                  `${config["firebaseJson"]} error. Hosting item as position ${Belt.Int.toString(
                      index,
                    )} does not have required "site" field.`,
                )
              | Some(siteConfig) => siteConfig === site
              }
            }
          }, hosting)) {
        | None =>
          Js.Exn.raiseError(
            `${config["firebaseJson"]} error. No "hosting[].site" match for ${config["hostingSite"]}.`,
          )
        | Some(hostingSite) => hostingSite
        }
      }

      switch Js.Json.decodeObject(matchedSite) {
      | None =>
        Js.Exn.raiseError(
          `${config["firebaseJson"]} error. Hosting match is not a valid JSON object.`,
        )
      | Some(siteConfig) =>
        switch Js.Dict.get(siteConfig, "rewrites") {
        | None =>
          Js.Exn.raiseError(
            `${config["firebaseJson"]} error. Hosting config does not have required "rewrites":[] field.`,
          )
        | Some(rewriteRules) =>
          switch Js.Json.decodeArray(rewriteRules) {
          | None =>
            Js.Exn.raiseError(
              `${config["firebaseJson"]} error. Hosting config "rewrites":[] field is not an array.`,
            )
          | Some(rules) =>
            switch Js.Array.findi((item, index) => {
              switch Js.Json.decodeObject(item) {
              | None =>
                Js.Exn.raiseError(
                  `${config["firebaseJson"]} error. "rewrites" item at position ${Belt.Int.toString(
                      index,
                    )} is not an object`,
                )
              | Some(rule) =>
                switch Js.Dict.get(rule, "source") {
                | None =>
                  Js.Exn.raiseError(
                    `${config["firebaseJson"]} error. Missing "source" field in rewrite rules.`,
                  )
                | Some(source) =>
                  Js.log2("config.sourceRewriteMatch", config["sourceRewriteMatch"])
                  Js.log2("source", source)
                  config["sourceRewriteMatch"] === source &&
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
            }, rules) {
            | None =>
              Js.Exn.raiseError(
                `${config["firebaseJson"]} error. No rewrites item matching "source":"${Js.Json.stringify(
                    config["sourceRewriteMatch"],
                  )}". The rule must contain either "function":"" or "run":{} configuration as well.`,
              )
            | Some(rule) => rule
            }
          }
        }
      }
    // Js.Dict.get(hostingConfig, "site")
    // switch Js.Json.classify(hostingConfig) {
    // | Js.Dict => Js.Dict.get(hostingConfig, "site") // returns the default "hosting"
    // | Js.Json.JSONArray => hostingConfig // returns "hosting[*]" that matches hostingSite
    // | _ =>
    //   Js.Exn.raiseError(
    //     `${config["firebaseJson"]} error. Expected "hosting" property to be "array" or "object."`,
    //   )
    // }
    | None => Js.Exn.raiseError(`${config["firebaseJson"]} error. Expected "hosting" property.`)
    }
  | _ =>
    Js.Exn.raiseError(`${config["firebaseJson"]} error. Expected root level JSON to be an object.`)
  }

  Js.log2("rewriteConfiguration", rewriteConfiguration)

  // Js.log2("builder>", builder)
  // Js.log2("firebase config>", config)
  // Js.log2("firebase json>", json)
  // Js.log(copy)
  // Js.Promise.make((~resolve, ~reject) => resolve(. 2))
}

// adapter function needs to be the default export
let default = adapter

// %%raw(`
// async function adapter(builder, {
// 	hostingSite = null,
// 	sourceRewriteMatch = '**',
// 	firebaseJson = 'firebase.json',
// 	cloudRunBuildDir = null
// } = {}) {
// 	const firebaseConfig = validateFirebaseConfig(
// 		firebaseJson,
// 		hostingSite,
// 		sourceRewriteMatch
// 	);

// 	// @ this point Joi has validated that all required values are in the config file
// 	const firebaseSiteConfig =
// 		firebaseConfig.hosting.length === 1 ?
// 			firebaseConfig.hosting[0] : // Site field is optional for single site configs, so just grab first
// 			firebaseConfig.hosting.find(item => item.site === hostingSite);
// 	const firebaseRewriteConfig = firebaseSiteConfig.rewrites.find(item => {
// 		return item.source === sourceRewriteMatch && (item.function || item.run);
// 	});

// 	if (firebaseRewriteConfig.run) {
// 		const serverOutputDir = Path.join(
// 			cloudRunBuildDir || '.'+firebaseRewriteConfig.run.serviceId
// 		);

// 		builder.log.minor('Writing Cloud Run service to ./'+serverOutputDir);
// 		builder.copy_server_files(serverOutputDir);
// 		copy(Path.join(__dirname, './files'), serverOutputDir);
// 		builder.log.warn(
// 			'To deploy your Cloud Run service, run this command:'
// 			+ '+--------------------------------------------------+'
// 			+ 'gcloud beta run --platform managed --region us-central1 deploy '
// 			+ firebaseRewriteConfig.run.serviceId
// 			+ ' --source '
// 			+ serverOutputDir
// 			+ ' --allow-unauthenticated'
// 			+ '+--------------------------------------------------+'
// 		);
// 	} else {
// 		throw new Error(
// 			'This code path should be unreachable, please open an issue @ https://github.com/jthegedus/svelte-adatper-firebase/issues with debug information'
// 		);
// 	}
// }
// `)
