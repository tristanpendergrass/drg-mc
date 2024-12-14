Demo at https://www.tristanpendergrass.com/drg-mc.

# Development

```
$ npm install
$ npm start -- If on Mac, use npm start-mac
```

# Deployment

## For first deploy or when changing url
There's some steps to follow for the first deploy to modify from the elm-empty-project name to the new name:
* In `README.md` modify the url for the demo
* In `package.json` modify parcel:build to have the right url
* In `index.html` modify page title to correct title

## Deploying to Github pages
* `$ npm run build` or `$ npm run build-mac`: This command builds files in the /docs directory by default
* Push built files to Github
* Log into Github on an account that can edit settings of your project
* Your repo -> Settings -> Pages -> Build and Deployment -> Branch -> master branch, /docs folder

# Testing

```
$ npm test
$ npm test path/to/file
$ npm test path/to/file -- --watch
```

# Defaults
* [Tailwind 3.*](https://tailwindcss.com/) loaded automatically (which includes a css reset)
* [DaisyUI](https://daisyui.com/docs/install/) loaded automatically
* [Feather Icons](https://feathericons.com/) loaded automatically via [elm-feather](https://github.com/feathericons/elm-feather)

# Todos

# v?
- [ ] Show unlock modal list
- [ ] Add elm-review
- [ ] Debug: allow buttons to have no cd

# v0.2
- [x] Add animation of resource gained after click
- [x] Add right sidebar showing dwarf levels
- [x] Add tab system on left
- [x] Add tab where dwarfs can be leveled
- [x] Finalize unlocks of new features
- [x] Add tab selection and color scheme to local storage
- [x] Hide dwarf display sidebar if not unlocked dwarf xp buttons
- [x] Automated tests for saving

# v0.1
- [x] Finalize leveling schedule
- [x] Add message advertising "next unlock at.."
- [x] Fav icon
- [x] Credits icon
- [x] Polish table styling so it doesn't change shape when clicking button
- [x] Change table: no yield column, button displays "Claim $1m cargo", button displays cooldown and disabled
- [x] Add game speed tuner
- [x] Animate xp bar
- [x] Add hazard symbol for haz levels