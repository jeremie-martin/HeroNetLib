// swift-tools-version:5.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
  name: "HeroNetLib",
  products: [ // Products define the executables and libraries produced by a package, and make them visible to other packages.
    .library(name: "HeroNetLib", targets: ["HeroNetLib"]),
    .executable(name: "HeroNet", targets: ["HeroNet"]),
  ], dependencies: [
    .package(url: "https://github.com/kyouko-taiga/PetriKit.git", from: "2.0.0"),
    .package(
      url: "https://github.com/kyouko-taiga/SwiftProductGenerator.git",
      from: "1.0.1"
    ),
    .package(
      url: "https://github.com/jeremie-martin/AlpineLang.git",
      .branch("master")
    ),
    .package(url: "https://github.com/kyouko-taiga/DDKit.git", .branch("master")),
  ], targets: [
    // Targets are the basic building blocks of a package. A target can define a module or a test suite.
    // Targets can depend on other targets in this package, and on products in packages which this package depends on.
    .target(name: "HeroNet", dependencies: ["HeroNetLib"]),
    .target(
      name: "HeroNetLib", dependencies: [
        "PetriKit",
        "SwiftProductGenerator",
        .product(name: "AlpineLib", package: "AlpineLang"),
        .product(name: "DDKit", package: "DDKit"),
      ]
    ),
    .testTarget(name: "HeroNetLibTests", dependencies: ["HeroNetLib"]),
  ]
)
