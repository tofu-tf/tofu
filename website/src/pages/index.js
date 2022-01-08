
import React from 'react';
import Link from '@docusaurus/Link';
import Translate, {translate} from '@docusaurus/Translate';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';

import Image from '@theme/IdealImage';
import Layout from '@theme/Layout';
import clsx from 'clsx';
import styles from './styles.module.css';

function Home() {
  const context = useDocusaurusContext();
  const {siteConfig: {customFields = {}, tagline} = {}} = context;
  return (
    <Layout title={tagline} description={customFields.description}>
      <main>
        <div className={styles.hero}>
          <div className={styles.heroInner}>
            <h1 className={styles.heroProjectTagline}>
              <img
                alt={translate({message: 'Tofu logo'})}
                className={styles.heroLogo}
                src={useBaseUrl('/img/tofu_logo_small_3.png')}
              />
              <span
                className={styles.heroTitleTextHtml}
                dangerouslySetInnerHTML={{
                  __html: translate({
                    id: 'homepage.hero.title',
                    message:
                      '<b>Functional</b> programming toolkit <b>aimed at taming</b> the complexity of <b>Tagless Final</b> approach.',
                    description:
                      'Home page hero title, can contain simple html tags',
                  }),
                }}
              />
            </h1>
            <div className={styles.indexCtas}>
              <Link className="button button--primary" to="/docs">
                <Translate>Get started</Translate>
              </Link>
              <span className={styles.indexCtasGitHubButtonWrapper}>
                <iframe
                  className={styles.indexCtasGitHubButton}
                  src="https://ghbtns.com/github-btn.html?user=tofu-tf&amp;repo=tofu&amp;type=star&amp;count=true&amp;size=large"
                  width={160}
                  height={30}
                  title="GitHub Stars"
                />
              </span>
            </div>
          </div>
        </div>
        <div className={clsx(styles.announcement, styles.announcementDark)}>
          <div className={styles.announcementInner}>
            <Translate
              values={{
                ce2Link: (
                  <Link to="https://typelevel.org/cats-effect/docs/2.x/getting-started">
                    <Translate>Cats Effect 2</Translate>
                  </Link>
                ),
                migrationGuideLink: (
                  <Link to="https://t.me/tofu_ru">
                    <Translate>CE2 to CE3 migration guide</Translate>
                  </Link>
                ),
              }}>
              {`Coming from {ce2Link} ? Check out our {migrationGuideLink}`}
            </Translate>
            .
          </div>
        </div>
        <div className={styles.section}>
          <div className="container text--left margin-top--xl">
            <div className="row">
              <div className="col text--center col--4">
                <img
                  className={styles.featureImageLarge}
                  alt="Powered by MDX"
                  src={useBaseUrl('/img/tofu_front_purple.png')}
                />
              </div>
              <div className="col">
                <h1 className={clsx(styles.featureHeading)}>
                  <Translate>Functional</Translate>
                </h1>
                <p className="index__text">
                  <Translate>
                  Functional programming is a programming paradigm in which it is tried to bind each and everything in pure mathematical functions. It is a declarative type of programming style that focuses on what to solve rather than how to solve (aimed by the imperative style of programming).
                  </Translate>
                </p>
              </div>
            </div>
          </div>
        </div>
        <div className={styles.section}>
          <div className="container text--left">
            <div className="row">
              <div className="col">
                <h1 className={clsx(styles.featureHeading)}>
                  <Translate>Tagless Final</Translate>
                </h1>
                <p className="index__text">
                  <Translate>
                  The pattern, born in the Haskell community, lets us embed a DSL into a host language. Even though the semantic has diverged from the original in Scala, the pattern’s main aim is to use interfaces as much as possible. 
                  </Translate>
                </p>
              </div>
              <div className="col text--center col--4">
                <img
                  className={styles.featureImageLarge}
                  alt="Powered by MDX"
                  src={useBaseUrl('/img/tofu_front_red.png')}
                />
              </div>
            </div>
          </div>
        </div>
        <div className={styles.section}>
          <div className="container text--left margin-bottom--xl">
            <div className="row">
              <div className="col text--center col--4">
                <img
                  className={styles.featureImageLarge}
                  alt="Powered by MDX"
                  src={useBaseUrl('/img/tofu_front_yellow.png')}
                />
              </div>
              <div className="col">
                <h1 className={clsx(styles.featureHeading)}>
                  <Translate>Modular</Translate>
                </h1>
                <p className="index__text">
                  <Translate>
                  Tired of vendor-locking? Tofu is an ecosystem of independent libraries and frameworks allowing you to significantly increase productivity of your Scala development.
                  </Translate>
                </p>
              </div>
            </div>
          </div>
        </div>
      </main>
    </Layout>
  );
}

export default Home;