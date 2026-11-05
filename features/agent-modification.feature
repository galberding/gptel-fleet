Feature: Modifying agents
  In order to delegate cheaply and safely
  As the person running the fleet
  I want to rewire agents quickly, and review agent-requested changes

  Scenario: Give a subagent another model from the agent list
    Given a fleet session for scenario "delegate" with inline approvals
    When I call "gptel-fleet-list-agents"
    And I go to the agent "researcher"
    And I start an action chain
    And I press "M"
    And I type "FleetMock:big"
    And I press "RET"
    And I execute the action chain
    Then agent "researcher" has :model big
    And I should see "FleetMock"

  Scenario: Add a tool to a subagent with the checklist
    Given a fleet session for scenario "delegate" with inline approvals
    When I call "gptel-fleet-list-agents"
    And I go to the agent "researcher"
    And I press "t"
    Then I should be in buffer "*gptel-fleet tools: researcher*"
    When I go to the checklist item "Grep"
    And I press "m"
    And I press "C-c C-c"
    Then agent "researcher" has :tools ("Read" "Bash" "Grep")

  Scenario: Let a subagent inherit its tools again
    Given a fleet session for scenario "delegate" with inline approvals
    When I call "gptel-fleet-list-agents"
    And I go to the agent "researcher"
    And I press "t"
    And I start an action chain
    And I press "i"
    And I type "y"
    And I execute the action chain
    Then agent "researcher" has no :tools

  Scenario: An agent asks to change another agent; I review and approve
    Given a fleet session for scenario "fleet" with inline approvals
    When I send the prompt
    Then I wait until buffer "*fleet-chat*" shows "PENDING"
    And the change queue holds 1 request
    And agent "researcher" has no :profile
    When I open the approval queue
    And I go to the row for "fleet-change"
    And I press "RET"
    Then I wait until buffer "*gptel-fleet call*" shows "agents/researcher.eld"
    When I switch to buffer "*gptel-fleet approvals*"
    And I go to the row for "fleet-change"
    And I press "y"
    Then agent "researcher" has :profile readonly
    And the change queue holds 0 requests
    And no window shows "*gptel-fleet call"
