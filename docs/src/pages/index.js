import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
import CodeBlock from '@theme/CodeBlock';

const example1 = `\
def foo():
  return test
`;

function Code({ lang, content }) {
    return (
      <div style={{textAlign: 'left'}}>
        <CodeBlock
          metastring={lang}
          className={lang}
        >{content}</CodeBlock>
      </div>
    );
}

function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="Monty programming language <head />">
      <header style={{height: '80%'}} className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className={clsx('container', styles.alignHeaderContent)}>
          <div>
            <h1 className="hero__title">{siteConfig.title}</h1>
            <p className="hero__subtitle">{siteConfig.tagline}</p>
            <div className={styles.buttons}>
              <Link
                className={clsx(
                  'button button--outline button--secondary button--lg',
                  styles.getStarted,
                )}
                to={useBaseUrl('docs/')}>
                Get Started
              </Link>
            </div>
          </div>
          <Tabs
            defaultValue="apple"
            values={[
                {label: 'Apple', value: 'apple'},
                {label: 'Orange', value: 'orange'},
                {label: 'Banana', value: 'banana'},
            ]}>
            <TabItem value="apple">
              <Code lang='python' content={example1} />
            </TabItem>
            <TabItem value="orange">This is an orange 🍊</TabItem>
            <TabItem value="banana">This is a banana 🍌</TabItem>
          </Tabs>
        </div>
      </header>
    </Layout>
  );
}

export default Home;
