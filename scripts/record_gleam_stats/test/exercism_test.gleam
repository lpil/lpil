import gleeunit/should
import script/exercism

const track_build_html = "
<!DOCTYPE html>
<html lang='en-US'>

<head>
  <meta content='text/html; charset=utf-8' http-equiv='Content-Type'>
</head>

<body class='namespace-tracks controller-build action-show theme-light user-signed_in'>
  <header id=\"site-header\"><a class=\"announcement-bar\" href=\"/donate\">
      <div class=\"lg-container\"><span>⚠️ Exercism needs donations to survive 2023. </span><strong>Please support us if
          you can!</strong><span>⚠️</span></div>
    </a>
    <div class=\"lg-container container\"><a class=\"exercism-link xl:block\" data-turbo-frame=\"tf-main\" href=\"/\"><img
          alt=\"Exercism\" class=\"c-icon\"
          src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/exercism-with-logo-black-b427c06c6a068ba9f391734115e4d22dfa876d1d.svg\" /></a>
      <div class=\"docs-search\">
        <div class=\"c-search-bar\"><input class=\"--search\" placeholder=\"Search Exercism&#39;s docs...\"></div>
      </div>
      <nav class=\"signed-in\">
        <ul>
          <li><a data-turbo-frame=\"tf-main\" class=\"relative\" href=\"/dashboard\"><img role=\"presentation\" alt=\"\"
                class=\"c-icon\"
                src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/dashboard-d6d946e5b42a4aef577066eca7b695641e21130a.svg\" /><span>Dashboard</span></a>
          </li>
          <li><a data-turbo-frame=\"tf-main\" class=\"relative\" href=\"/tracks\"><img role=\"presentation\" alt=\"\"
                class=\"c-icon\"
                src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/tracks-2e780b460e113a9b07ce4446c988a31c40547b00.svg\" /><span>Tracks</span></a>
          </li>
          <li><a data-turbo-frame=\"tf-main\" class=\"relative\" href=\"/mentoring/inbox\"><img role=\"presentation\" alt=\"\"
                class=\"c-icon\"
                src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/mentoring-4d1e266458e8e3293b94679b92ccea4aa2cdb58b.svg\" /><span>Mentoring</span></a>
          </li>
          <li><a data-turbo-frame=\"tf-main\" class=\"relative\" href=\"/community\"><img role=\"presentation\" alt=\"\"
                class=\"c-icon\"
                src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/community-97592cd275dfff37bf718b70931fc1572363f29f.svg\" /><span>Community</span>
              <div
                class=\"ml-8 text-warning bg-lightOrange px-8 py-6 rounded-100 font-semibold text-[13px] flex items-center\">
                <img role=\"presentation\" alt=\"\" class=\"c-icon !filter-warning !w-[12px] !h-[12px] !mr-4 !block\"
                  src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/sparkle-0eb827302d20b3ed56848b2766f0fa70c7702389.svg\" /><span>New</span>
              </div>
            </a></li>
          <li><a data-turbo-frame=\"tf-main\" class=\"relative\" href=\"/donate\"><img role=\"presentation\" alt=\"\"
                class=\"c-icon\"
                src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/contribute-269d926a07a0b503203914911e0a5704469ae8c4.svg\" /><span>Donate
                💜</span></a></li>
        </ul>
      </nav>
      <div class=\"user-section\">
        <div class=\"c-react-component c-react-wrapper-dropdowns-notifications\" data-react-id=\"dropdowns-notifications\"
          data-react-data=\"{&quot;endpoint&quot;:&quot;https://exercism.org/api/v2/notifications?order=unread_first&quot;}\"
          data-react-hydrate=\"false\"></div>
      </div>
    </div>
  </header>
  <turbo-frame data-turbo-action=\"advance\" id=\"tf-main\">
    <div class='bg-white' id='page-tracks-build'>
      <header class=\"c-track-header\">
        <div class='lg-container container'>
          <div class='flex items-center'>
            <div class='flex-grow'>
              <div class='flex items-center relative lg:static'>
                <div class='flex flex-col md:flex-row md:items-center'>
                  <div class='text-h1 md:mr-24'>Gleam</div>
                  <div class='students hidden lg:flex items-center'>
                    <img role=\"presentation\" alt=\"\" class=\"c-icon\"
                      src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/students-caf4c344871c7bb1e0988f33a3ead0944160d4e0.svg\" />
                    <span>
                      272
                      students
                    </span>
                  </div>
                  <div class='tags lg:hidden mt-8'></div>
                </div>
              </div>
              <div class='tabs'><a class=\"c-tab-2 \" href=\"/tracks/gleam\"><img role=\"presentation\" alt=\"\" class=\"c-icon\"
                    src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/overview-9e9a39e9f2e81d2a28adf4254c588f9864d7e3a3.svg\" /><span>Overview</span></a><a
                  class=\"c-tab-2 \" href=\"/tracks/gleam/exercises\"><img role=\"presentation\" alt=\"\" class=\"c-icon\"
                    src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/exercises-8a7df249fbfb76cc18efbbee844d9bd742830404.svg\" /><span>Exercises</span></a><a
                  class=\"c-tab-2 \" href=\"/tracks/gleam/about\"><img role=\"presentation\" alt=\"\" class=\"c-icon\"
                    src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/info-circle-0db7a70f51493909ba515887222c1c7cf27e02ad.svg\" /><span>About</span></a><a
                  class=\"c-tab-2 selected\" href=\"/tracks/gleam/build\"><img role=\"presentation\" alt=\"\" class=\"c-icon\"
                    src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/building-063652461094d5ce92b137453e734fcff2caaf68.svg\" /><span>Build
                    Status</span></a></div>
            </div>
            <div class='mr-84 hidden lg:flex items-center ml-auto'>
              <div class='tags mr-32'></div>
              <a class=\"people hidden lg:flex \" href=\"/contributing/contributors?track_slug=gleam\">
                <div class='c-faces'>
                  <div class='face'>
                    <div class=\"c-avatar\"
                      style=\"background-image:url(&quot;https://avatars2.githubusercontent.com/u/6134406&quot;)\"><img
                        alt=\"Uploaded avatar of lpil\" class=\"sr-only\"
                        src=\"https://avatars2.githubusercontent.com/u/6134406\" /></div>
                  </div>
                  <div class='face'>
                    <div class=\"c-avatar\"
                      style=\"background-image:url(&quot;https://avatars.githubusercontent.com/u/16929078?v=4&quot;)\">
                      <img alt=\"Uploaded avatar of jiegillet\" class=\"sr-only\"
                        src=\"https://avatars.githubusercontent.com/u/16929078?v=4\" />
                    </div>
                  </div>
                  <div class='face'>
                    <div class=\"c-avatar\"
                      style=\"background-image:url(&quot;https://avatars3.githubusercontent.com/u/135246&quot;)\"><img
                        alt=\"Uploaded avatar of ErikSchierboom\" class=\"sr-only\"
                        src=\"https://avatars3.githubusercontent.com/u/135246\" /></div>
                  </div>
                </div>
                <div class='stats'>
                  <div class='contributors'>17 contributors</div>
                  <div class='mentors'>20 mentors</div>
                </div>
              </a>
            </div>
          </div>
        </div>
      </header>
      <div class='track-build-header'></div>
      <div class='lg-container grid grid-cols-3 gap-48 relative mt-40'>
        <div class='col-span-3 xl:col-span-2'>
          <h1 class='text-h1 mb-16 text-40 font-bold z-1'>
            <div class='highlight-text'>267</div>
            students have developed their Gleam skills on Exercism.
          </h1>
          <div class='grid grid-cols-3 mb-32 gap-16'>
            <div class='report-stat'>
              <div class='stat-name'>
                Students
                <img role=\"presentation\" alt=\"\" class=\"c-icon\"
                  src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/info-circle-0db7a70f51493909ba515887222c1c7cf27e02ad.svg\" />
              </div>
              <div class='current-number'>267</div>
            </div>
            <div class='report-stat'>
              <div class='stat-name'>
                Submissions
                <img role=\"presentation\" alt=\"\" class=\"c-icon\"
                  src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/info-circle-0db7a70f51493909ba515887222c1c7cf27e02ad.svg\" />
              </div>
              <div class='current-number'>4,637</div>
            </div>
            <div class='report-stat'>
              <div class='stat-name'>
                Mentoring Discussions
                <img role=\"presentation\" alt=\"\" class=\"c-icon\"
                  src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/info-circle-0db7a70f51493909ba515887222c1c7cf27e02ad.svg\" />
              </div>
              <div class='current-number'>
                51
              </div>
            </div>
          </div>
          <div class='track-status'>
            <h3 class='text-h3 font-bold mb-12'>
              The Gleam track
              <span class=\"text-warning\">needs attention ⚠️</span>
            </h3>
            <div class='flex'>
              <div class='flex flex-col mr-40'>
                <div class='tooling-status-label'>Building</div>
                <div class='tooling-status-group'>
                  <div aria-label='Status of the test runner for Gleam'
                    data-endpoint='/tracks/gleam/build/test_runner_tooltip' data-tooltip-type='tooling'
                    tooling-status='exemplar'>
                    <img role=\"presentation\" alt=\"\" class=\"c-icon\"
                      src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/test-runner-321f2fc18543bb0002cd2afd4a2ff3ddbf707afc.svg\" />
                  </div>
                  <div aria-label='Status of the analyzer for Gleam'
                    data-endpoint='/tracks/gleam/build/analyzer_tooltip' data-tooltip-type='tooling'
                    tooling-status='missing'>
                    <img role=\"presentation\" alt=\"\" class=\"c-icon\"
                      src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/analyzer-5c625174ec715bc9a879ccae279661d37261c589.svg\" />
                  </div>
                  <div aria-label='Status of the representer for Gleam'
                    data-endpoint='/tracks/gleam/build/representer_tooltip' data-tooltip-type='tooling'
                    tooling-status='missing'>
                    <img role=\"presentation\" alt=\"\" class=\"c-icon\"
                      src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/representer-c8cabd348c4fa9543972769834dcc3c51acf37f5.svg\" />
                  </div>
                  <div aria-label='Practice Exercises status for Gleam'
                    data-endpoint='/tracks/gleam/build/practice_exercises_tooltip' data-tooltip-type='tooling'
                    tooling-status='exemplar'>
                    <img role=\"presentation\" alt=\"\" class=\"c-icon\"
                      src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/practice-exercises-6f6f10f3dd3e2e62e10cb31b4752430469818aa4.svg\" />
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div class='flex mb-16 items-center'>
            <div class=\"c-icon h-[64px] w-[64px] mr-20 --hex\"><img role=\"presentation\" alt=\"\"
                src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/building-063652461094d5ce92b137453e734fcff2caaf68.svg\" />
            </div>
            <div class='flex flex-col mr-auto'>
              <h2 class='text-h2'>Build</h2>
              <p class='text-18 text-textColor1 leading-150'>Use your knowledge to build the Gleam track itself</p>
            </div>
            <a class=\"people hidden lg:flex items-center\" href=\"/contributing/contributors?track_slug=gleam\">
              <div class='c-faces mr-16'>
                <div class='face'>
                  <div class=\"c-avatar\"
                    style=\"background-image:url(&quot;https://avatars2.githubusercontent.com/u/6134406&quot;)\"><img
                      alt=\"Uploaded avatar of lpil\" class=\"sr-only\"
                      src=\"https://avatars2.githubusercontent.com/u/6134406\" /></div>
                </div>
                <div class='face'>
                  <div class=\"c-avatar\"
                    style=\"background-image:url(&quot;https://avatars.githubusercontent.com/u/16929078?v=4&quot;)\"><img
                      alt=\"Uploaded avatar of jiegillet\" class=\"sr-only\"
                      src=\"https://avatars.githubusercontent.com/u/16929078?v=4\" /></div>
                </div>
                <div class='face'>
                  <div class=\"c-avatar\"
                    style=\"background-image:url(&quot;https://avatars3.githubusercontent.com/u/135246&quot;)\"><img
                      alt=\"Uploaded avatar of ErikSchierboom\" class=\"sr-only\"
                      src=\"https://avatars3.githubusercontent.com/u/135246\" /></div>
                </div>
              </div>
              <div class='stats text-16'>
                <div class='contributors text-textColor1 font-medium mb-2'>17 contributors</div>
              </div>
            </a>
          </div>
          <div class='track-team-group'>
            <div class='track-header mb-16'>
              <div class='flex items-center justify-between mb-8'>
                <h3 class='--test-runner-gradient'>Build Test Runners</h3>
                <a class=\"learn-more-new-tab\" href=\"/docs/building/tooling/test-runners\">Learn More
                </a>
              </div>
              <p>Create Test Runners that have the single responsibility of taking a solution, running all tests and
                returning a standardized output.</p>
            </div>
            <div class='action-required'>
              Next goal: Get Test Runners to Version 2. Gleam is currently utilising a Version 1 Test Runner.
              <a href=\"/docs/building/tooling/test-runners/interface\">Find out more.
              </a>
            </div>
            <div class='usage-stats-header'>
              <h4>Usage statistics</h4>
            </div>
            <div class='stats-container test-runner'>
              <div class='record-row'>
                <div class='record-name'>Total Test Runner Runs</div>
                <div class='record-value'>4,637</div>
              </div>
              <div class='record-row'>
                <div class='record-name'>Test Run Pass Rate</div>
                <div class='record-value'>31.9% (1478)</div>
              </div>
              <div class='record-row'>
                <div class='record-name'>Test Run Fail Rate</div>
                <div class='record-value'>18.5% (859)</div>
              </div>
              <div class='record-row'>
                <div class='record-name'>Test Run Error Rate</div>
                <div class='record-value'>49.6% (2300)</div>
              </div>
            </div>
          </div>
          <div class='track-team-group'>
            <div class='track-header mb-16'>
              <div class='flex items-center justify-between mb-8'>
                <h3 class='--analyzer-gradient'>Build Analyzers</h3>
                <a class=\"learn-more-new-tab\" href=\"/docs/building/tooling/analyzers\">Learn More
                </a>
              </div>
              <p>Build an Analyzer for Gleam: Exercism's analyzers automatically assess student's submissions and
                provide mentor-style commentary.</p>
            </div>
            <div class='action-required'>
              Next goal: Build an Analyzer.
              <a href=\"/docs/building/tooling/analyzers/interface\">Find out more.
              </a>
            </div>
            <div class=\"c-react-component c-react-wrapper-common-credits font-semibold\" data-react-id=\"common-credits\"
              data-react-data=\"{&quot;users&quot;:[],&quot;top_count&quot;:0,&quot;top_label&quot;:&quot;contributor&quot;,&quot;bottom_count&quot;:0,&quot;bottom_label&quot;:&quot;&quot;}\"
              data-react-hydrate=\"false\"></div>
          </div>
          <div class='track-team-group'>
            <div class='track-header mb-16'>
              <div class='flex items-center justify-between mb-8'>
                <h3 class='--representer-gradient'>Build Representers</h3>
                <a class=\"learn-more-new-tab\" href=\"/docs/building/tooling/representers\">Learn More
                </a>
              </div>
              <p>Build a Representer: a bit of code that has the single responsibility of taking a solution and
                returning a normalized representation of it.</p>
            </div>
            <div class='action-required'>
              Next goal: Build a Representer.
              <a href=\"/docs/building/tooling/representers/interface\">Find out more.
              </a>
            </div>
            <div class=\"c-react-component c-react-wrapper-common-credits font-semibold\" data-react-id=\"common-credits\"
              data-react-data=\"{&quot;users&quot;:[],&quot;top_count&quot;:0,&quot;top_label&quot;:&quot;contributor&quot;,&quot;bottom_count&quot;:0,&quot;bottom_label&quot;:&quot;&quot;}\"
              data-react-hydrate=\"false\"></div>
          </div>
          <div class='track-team-group'>
            <div class='track-header mb-16'>
              <div class='flex items-center justify-between mb-8'>
                <h3 class='--practice-exercises-gradient'>Create Practice Exercises</h3>
                <a class=\"learn-more-new-tab\" href=\"/docs/building/tracks/new/add-initial-exercises\">Learn More
                </a>
              </div>
              <p>Practice Exercises are exercises designed to allow students to solve an arbitrary problem, with the aim
                of them making use of the concepts they have learned so far.</p>
            </div>
            <div class='action-required'>
              Next goal: Implement 129 practice exercises.
              <a href=\"/docs/building/tracks/syllabus/next-exercises\">Find out more.
              </a>
            </div>
            <div class='usage-stats-header'>
              <h4>Usage statistics</h4>
            </div>
            <details>
              <summary class='--practice-exercises'>
                78 active practice exercises
                <img role=\"presentation\" alt=\"\" class=\"c-icon summary-chevron\"
                  src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/chevron-right-4edf20ec1116acf2e371e8ff03809451274a7b32.svg\" />
              </summary>
              <div class='record-row sticky top-[106px] z-1 lg:top-0'>
                <div class='record-name'></div>
                <div class='record-value'>
                  <div class='record-element'>Started</div>
                  <div class='record-element'>Attempts</div>
                  <div class='record-element'>Completions</div>
                  <div class='record-element'>Mentoring requests</div>
                </div>
              </div>
              <div class='record-row'>
                <div class='record-name'>
                  <img alt=\"Hello World\" class=\"c-icon c-exercise-icon \"
                    src=\"https://dg8krxphbh767.cloudfront.net/exercises/hello-world.svg\" />
                  Hello World
                </div>
                <div class='record-value'>
                  <div class='record-element'>
                    <strong>230</strong>
                  </div>
                  <div class='record-element'>
                    <strong>292 (avg. 1.3)</strong>
                  </div>
                  <div class='record-element'>
                    <strong>212 (92.2%)</strong>
                  </div>
                  <div class='record-element'>
                    <strong>0 (0.0%)</strong>
                  </div>
                </div>
              </div>
              <div class='record-row'>
                <div class='record-name'>
                  <img alt=\"Resistor Color\" class=\"c-icon c-exercise-icon \"
                    src=\"https://dg8krxphbh767.cloudfront.net/exercises/resistor-color.svg\" />
                  Resistor Color
                </div>
                <div class='record-value'>
                  <div class='record-element'>
                    <strong>154</strong>
                  </div>
                  <div class='record-element'>
                    <strong>380 (avg. 2.5)</strong>
                  </div>
                  <div class='record-element'>
                    <strong>106 (68.8%)</strong>
                  </div>
                  <div class='record-element'>
                    <strong>5 (3.2%)</strong>
                  </div>
                </div>
              </div>
            </details>
          </div>
        </div>
        <div class='hidden xl:block col-span-1'>
          <div class='rounded-8 p-24 shadow-baseZ1 bg-white'>
            <div class='flex items-center mb-12'>
              <img role=\"presentation\" alt=\"\" class=\"c-icon h-[24px] w-[24px] filter-textColor1 mr-16\"
                src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/team-idea-36b87f7739dd51206ce2cd28b7a5ab3b8f5df4f0.svg\" />
              <div class='label-large mr-16'>
                Volunteers
              </div>
              <div class='count-bubble'>19</div>
            </div>
            <div class='contributor-credit-block'>
              <div class='contributor'>
                <div class=\"h-32 w-[32px] bg-cover bg-center rounded-circle mr-12\"
                  style=\"background-image:url(&quot;https://avatars2.githubusercontent.com/u/6134406&quot;)\"><img
                    alt=\"Uploaded avatar of lpil\" class=\"sr-only\"
                    src=\"https://avatars2.githubusercontent.com/u/6134406\" /></div>
                lpil
              </div>
              <div class=\"c-primary-reputation --small\" aria-label=\"1,360 reputation\"><img alt=\"Reputation\"
                  class=\"c-icon\"
                  src=\"https://d24y9kuxp2d7l2.cloudfront.net/assets/icons/reputation-5b5938e36519908ac61075db3b9826307a0f907a.svg\" /><span>1,360</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>

  </turbo-frame>
</body>

</html>
"

pub fn parsing_test() {
  exercism.parse_track_page(track_build_html)
  |> should.equal(
    Ok(exercism.Information(
      students_count: 267,
      submissions_count: 4637,
      mentoring_discussions_count: 51,
    )),
  )
}
