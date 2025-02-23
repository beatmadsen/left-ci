export default {
  roots: ["<rootDir>/tests"],    // Run tests only in js/tests
  testEnvironment: "jsdom",         // Use a browser-like environment
  moduleDirectories: ["node_modules", "<rootDir>/lib"], // Resolve imports from js/lib
  extensionsToTreatAsEsm: [".js"],   // Treat .js files as ES modules  
};
