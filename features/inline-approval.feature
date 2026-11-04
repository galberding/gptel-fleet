Feature: Approving tool calls inline, the gptel way
  In order to stay in my chat while agents work
  As a gptel-agent user
  I want waiting tool calls to appear inline, never as popups

  Scenario: A subagent's call waits inline and is approved with C-c C-c
    Given a fleet session for scenario "delegate" with inline approvals
    When I send the prompt
    Then a tool call waits inline
    And the mode line says "Fleet:1"
    And no window shows "*gptel-fleet"
    And there is 1 window
    When I go to the tool call overlay
    And I press "C-c C-c"
    Then I wait until buffer "*fleet-chat*" shows "MAIN DONE"
    And the mode line says ""

  Scenario: The same call can be approved from the queue buffer
    Given a fleet session for scenario "delegate" with inline approvals
    When I send the prompt
    Then a tool call waits inline
    When I open the approval queue
    And I go to the row for "Read"
    Then the queue row says agent "researcher" tool "Read"
    When I press "y"
    Then I wait until buffer "*fleet-chat*" shows "MAIN DONE"
    And no window shows "*gptel-fleet approvals"

  Scenario: Rejecting inline leaves nothing pending
    Given a fleet session for scenario "delegate" with inline approvals
    When I send the prompt
    Then a tool call waits inline
    When I go to the tool call overlay
    And I press "C-c C-k"
    Then the mode line says ""
