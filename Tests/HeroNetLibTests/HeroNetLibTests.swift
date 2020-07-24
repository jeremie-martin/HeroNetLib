import XCTest
@testable import HeroNetLib

final class HeroNetLibTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(HeroNetLib().text, "Hello, World!")
    }

    static var allTests = [
        ("testExample", testExample),
    ]
}
