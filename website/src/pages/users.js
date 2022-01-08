import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import React from 'react';
import Layout from '@theme/Layout';
import styles from './styles.module.css';

function Users() {
    const context = useDocusaurusContext();
    const { siteConfig = {} } = context;
    
    const editUrl = `${siteConfig.repoUrl}/edit/master/website/docusaurus.config.js`;
    const showcase = siteConfig.customFields.users.map(user => (
      <a href={user.infoLink} key={user.infoLink}>
        <img className={styles.featureImageLarge} src={user.image} alt={user.caption} title={user.caption} />
      </a>
    ));

    return (
    <Layout title={siteConfig.tagline}>
    <main className={styles.main}>
      <div className="hero text--center">
        <div className="container">
          <div className="card">
            <div className="card__body">
            <div className="card__header">
              <h1>Who is Using This?</h1>
              <p>This project is used by many folks</p>
            </div>
            <div className="card__image">{showcase}</div>
            <p>Are you using this project?</p>
            <button onclick={editUrl} className="button button--secondary button--block">
              Add your company
            </button>
          </div>
         </div>
        </div>
      </div>
     </main>
    </Layout>
    );
};

export default Users;
