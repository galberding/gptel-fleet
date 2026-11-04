Feature: Queued calls and editing a call before it runs
  In order to fix a nearly-right command instead of rejecting it
  As a gptel-agent user
  I want to edit a waiting call in a proper buffer and submit it

  Scenario: Edit a Bash command in a shell buffer and submit with C-c '
    Given a fleet session for scenario "bash" with queue approvals
    When I send the prompt
    Then the change queue holds 1 request
    And no window shows "*gptel-fleet"
    And the mode line says "Fleet:1"
    When I open the approval queue
    And I go to the row for "Bash"
    And I press "e"
    Then I should be in buffer "*gptel-fleet edit: Bash command*"
    And the current buffer is in a mode derived from sh-base-mode
    When I replace the buffer contents with "echo RIGHT-COMMAND"
    And I press "C-c '"
    Then I wait until buffer "*fleet-chat*" shows "MAIN DONE"
    And I wait until buffer "*fleet-chat*" shows "user edited this call"
    And I wait until buffer "*fleet-chat*" shows "RIGHT-COMMAND"
    And the mode line says ""

  Scenario: Leaving the edit buffer keeps the call waiting
    Given a fleet session for scenario "bash" with queue approvals
    When I send the prompt
    Then the change queue holds 1 request
    When I open the approval queue
    And I go to the row for "Bash"
    And I press "e"
    And I press "C-c C-k"
    Then I should be in buffer "*gptel-fleet approvals*"
    And the mode line says "Fleet:1"
    When I go to the row for "Bash"
    And I press "n"
    Then I wait until buffer "*fleet-chat*" shows "denied"
