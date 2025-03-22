module.exports = {
  content: ["./src/**/*.{html,js,jsx,ts,tsx,elm}"],
  theme: {
    extend: {
      keyframes: {
        wiggle: {
          '0%, 100%': { transform: 'translateX(0px)' },
          '25%': { transform: 'translateX(-4px)' },
          '50%': { transform: 'translateX(0px)' },
          '75%': { transform: 'translateX(4px)' },
        },
        rocketUp: {
          '0%': { transform: 'translateY(150%)' },
          '100%': { transform: 'translateY(-50%)' },
        },
        rocketDown: {
          '0%': { transform: 'translateY(0)' },
          '100%': { transform: 'translateY(100%)' },
        },
        smokeRise: {
          '0%': {
            transform: 'translateY(0%) scale(1)',
            opacity: '1',
          },
          '100%': {
            transform: 'translateY(-300%) scale(1.5)',
            opacity: '0.25',
          },
        },
        'gooey-pulse': {
          '0%': { transform: 'scale(0.90)' },
          '40%': { transform: 'scale(1.06)' },
          '60%': { transform: 'scale(0.98)' },
          '80%': { transform: 'scale(1.01)' },
          '100%': { transform: 'scale(1)' },
        },
      },
      animation: {
        'rocket-enter': 'rocketUp 2s ease-out',
        'rocket-exit': 'rocketDown 0.5s ease-in',
        smokeRise: 'smokeRise 2s ease-out forwards',
        'gooey-pulse': 'gooey-pulse 0.6s ease-in-out'
      },
    },
  },
  daisyui: {
    themes: [
      "light",
      "dark",
      "cupcake",
      "bumblebee",
      "emerald",
      "corporate",
      "synthwave",
      "retro",
      "cyberpunk",
      "valentine",
      "halloween",
      "garden",
      "forest",
      "aqua",
      "lofi",
      "pastel",
      "fantasy",
      "wireframe",
      "black",
      "luxury",
      "dracula",
      "cmyk",
      "autumn",
      "business",
      "acid",
      "lemonade",
      "night",
      "coffee",
      "winter",
      "dim",
      "nord",
      "sunset",
    ],
  },
}