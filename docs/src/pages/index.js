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
result = unwrap:
  a = Just(3)
  b = Just(7)
  wrap(a + b)

result # Just(10)
`;

const example2 = `\
class Maybe:
  None
  Just(value)

def foo(Just(x)):
  return x+1

def foo(None):
  return -1

foo(Just(3)) # returns 4
foo(None) # returns -1
`;

const example3 = `\
type Functor:
  def map(self, function)

instance Maybe of Functor:
  def map(Just(value), f):
    return Just(f(value))

  def map(None, _):
    return None
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
      <header className={clsx('hero--primary', styles.heroBanner, styles.customHero)}>
        <div className={clsx('container', styles.alignHeaderContent)}>
          <div className={clsx(styles.spacer)}>
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
            defaultValue="monads"
            values={[
                {label: 'Monads', value: 'monads'},
                {label: 'Pattern Matching', value: 'patternMatching'},
                {label: 'Type Classes', value: 'typeClasses'},
            ]}>
            <TabItem value="monads">
              <Code lang='python' content={example1} />
            </TabItem>
            <TabItem value="patternMatching">
              <Code lang='python' content={example2} />
            </TabItem>
            <TabItem value="typeClasses">
              <Code lang='python' content={example3} />
            </TabItem>
          </Tabs>
        </div>
      </header>
    </Layout>
  );
}

export default Home;
