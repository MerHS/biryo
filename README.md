# biryo [![Build Status](https://travis-ci.org/MerHS/biryo.svg?branch=master)](https://travis-ci.org/MerHS/biryo)


## Screenshot

![Running Demo](https://thumbs.gfycat.com/TestyCandidIsabellinewheatear-size_restricted.gif)


## Library Dependencies

* Scala >= 2.11 (JDK >= 7)
* [jawn](https://github.com/non/jawn)
* [parboiled2](https://github.com/sirthias/parboiled2)
* [shapeless](https://github.com/milessabin/shapeless) (for parboiled2)
* [Akka](http://akka.io/)


## It supports...

* [NamuMark](https://namu.wiki/w/%EB%82%98%EB%AC%B4%EC%9C%84%ED%82%A4:%EB%AC%B8%EB%B2%95%20%EB%8F%84%EC%9B%80%EB%A7%90) Parser
* Mappable / Traversable NamuMark AST(Abstract Syntax Tree) (NamuAST.scala)
* HTML Generator(for MdxBuilder)

## NamuMark AST (`NamuAST.scala`)

### Supporting NamuMark Syntax

[나무마크 문법](https://namu.wiki/w/%EB%82%98%EB%AC%B4%EC%9C%84%ED%82%A4:%EB%AC%B8%EB%B2%95%20%EB%8F%84%EC%9B%80%EB%A7%90) 참조

* 일반 텍스트 (`RawString`, `Paragraph`)
* 3\. 문단 (`Headings`)
* 4\. 서식 (`SpanMark`)
  * 4.1. 텍스트 형태
    * 굵게 (`Bold`)
    * 기울임 (`Italic`)
    * 취소선 (`Strike`)
    * 밑줄 (`Underline`)
    * 위첨자 (`Sup`)
    * 아래첨자 (`Sub`)
  * 4.2. 텍스트 크기 (`SizeBlock`)
  * 4.3. 텍스트 색상 (`ColorBlock`)
  * 4.4. 인라인 코드 (`InlineString`)
* 5\. 하이퍼링크 (`DocLink(NamuHref, Option[NamuMark])`)
	* 5.1. 링크와 출력이 같은 하이퍼링크 (`NormalHref`)
	* 5.2. 링크와 출력이 다른 하이퍼링크 (`Some[NamuMark]`)
	* 5.3. 문서의 특정 문단으로 하이퍼링크 걸기 (`ParaHref`, `AnchorHref`, `SelfParaHref`, `SelfAnchorHref`)
	* 5.4. 상위, 하위 문서로 하이퍼링크 걸기 (`SuperDocHref`, `ChildDocHref`)
	* 5.5. 외부 페이지로 하이퍼링크 걸기 (`ExternalHref`)
	* 5.6. 앵커 (`Anchor`)
* 6\. 리다이렉트 (`Redirect`)
* 7\. 이미지 첨부
	* 7.1. 나무위키 자체 이미지 업로더 (`FileLink`)
	* 7.1.1. 이미지 크기 및 위치 조절 (`htmlOption`)
* 8\. 동영상 첨부
	* 8.1. 유튜브 동영상 첨부 (`YoutubeLink`)
* 9\. 글상자 (`WordBox`)
* 11\. 각주 (`FootNote`, `LinkOnlyFn`)
* 12\. 들여쓰기 (`Indent`)
* 13\. 인용문 (`BlockQuote`)
* 14\. 수평줄 (`HR`)
* 15\. 주석 (`Comment`)
* 16\. 매크로
	* 16.1. date (datetime) (`DateMacro`)
	* 16.2. br (`BR`)
	* 16.4. include (`Include`)
	* 16.5. 목차 (`TableOfContents`)
	* 16.6. 각주 (`FootNoteList`)
	* 16.7. 나이 (`AgeMacro`)
* 17\. 테이블(표) (`TableWrapper`)
	* 테이블의 각 항목은 `Table` `TR`, `TD`와 같은 HTML요소, `TableAlign`, `TableStyle`등의 CSS 요소로 구현되어 있음
* 20\. HTML (`HTMLString`)
	* 20.1. 틀 (mdd에 저장, 원본 또한 mdx에서 볼 수 있음)
* 21\. 문법 강조 (`SyntaxBlock`) (not-rendered)
* 22\. 분류 (`DocType`)

### Unsupported Functions

* 자동 리다이렉트
  * MDict에 자동 리다이렉트 기능이 구현되어있으나, 앵커 사용 불가 및 디버깅 용도 등으로 인해 해당 링크를 띄우는 식으로만 구현하였습니다. 
* 이미지, 동영상 표시
  * 이미지를 비롯한 외부 파일 링크는 모두 해당 표제어나 URL로 가는 텍스트 링크로 구현되어있습니다. 
* `[date]`, `[age]`등의 매크로 표시 
  * 파싱은 하나 표시는 되지 않습니다. JS로 구현할 계획
* 16.4. include (틀 제외)
  * 현재 틀은 외부 JS 형식으로 변환하여 mdd에 저장하여 읽고있기 때문에 mdx-only 버전에선 렌더링되지 않습니다.
* 21\. 문법 강조는 현재 4.4 인라인 코드와 같은 방식으로 렌더링됨
* 22\. 분류가 따로따로 나옴 
  * 현재는 분류 페이지에 해당 분류의 항목이 무엇이 있는지 보이지 않습니다. 분류들이 한곳에 모이지 않기 때문에 현재 JS 후처리를 통해 분류를 렌더링하지 않고 있습니다. 

### Unsupported Syntax

* 7.2. 외부 이미지 링크 (현재 지원 종료)
  * 구현 계획 없음
* 10\. 리스트
  * 들여쓰기는 구현되어 있으므로 리스트 첨자를 제외한 부분은 거의 비슷하게 보입니다.
* 16.3. pagecount
* 16.3.1. 네임스페이스를 지정한 pagecount
* 16.8. 남은 날 (dday)
* 18\. 수식
 Katex로 구현할까 고민 중
* 19\. 접기

## How to build an Executable JAR

1. [JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html) 설치
  * 환경변수 (PATH) 설정이 되어있지 않아 java 명령어가 작동하지 않으면 같이 해줍니다. 
2. [sbt](https://github.com/MerHS/biryo) 설치 (Scala 빌드를 위한 툴입니다.)

다음부터는 cmd, powershell, bash등 원하는 쉘에서 

3. `git clone https://github.com/MerHS/biryo`
4. `cd biryo`
5. `sbt assembly`

target/scala-2.11 폴더에 `biryo.jar`이 만들어집니다. 


## How to Use

**주의:** 안드로이드/PC버전 **MDict 1.x** 버전 사용자는 **MdxBuilder 3.0**
**MDict 2.0** 버전 사용자는 **MdxBuilder 4.0**버전으로 mdx/mdd파일을 만들어야 정상 작동합니다.

mdd를 읽지 못하는 일부 기기를 위해 CSS 코드가 각 항목마다 인라인되어있는 mdx only 버전을 제작할 수 있습니다.
일반적인 경우라면 mdx, mdd를 같이 만드시면 됩니다. (mdx only버전의 경우 파일크기가 약 200MB정도 커집니다. )

위에서 jar파일을 만들었거나 받았다고 가정하고 

0. jar파일이 있는 경로에 [mdict-data](https://github.com/MerHS/biryo/tree/master/mdict-data) 폴더를 넣습니다. 스타일링 및 JS파일을 위해 반드시 필요합니다. 
1. [나무위키 데이터베이스 덤프](https://namu.wiki/w/%EB%82%98%EB%AC%B4%EC%9C%84%ED%82%A4:%EB%8D%B0%EC%9D%B4%ED%84%B0%EB%B2%A0%EC%9D%B4%EC%8A%A4%20%EB%8D%A4%ED%94%84?from=%EB%82%98%EB%AC%B4%EC%9C%84%ED%82%A4%20%EB%8D%B0%EC%9D%B4%ED%84%B0%EB%B2%A0%EC%9D%B4%EC%8A%A4%20%EB%8D%A4%ED%94%84)를 받고 압축을 풀어 jar파일과 같은 경로에 넣습니다. (이름이 namuwiki.json 파일이라고 가정)
2. `java -Dfile.encoding=UTF-8 -jar biryo.jar namuwiki.json`
  * mdx only 버전용 HTML을 만들고 싶으면 `java -Dfile.encoding=UTF-8 -jar biryo.jar -inline namuwiki.json` 
3. 만들어진 `namu.txt` (또는 `namu_inline.txt`)를 MdxBuilder의 Source에, Data에는 `mdict-data`폴더의 경로를 넣고, Target에는 출력할 mdx파일의 경로 및 이름을 넣습니다.
  * mdx only 용은 Data에 아무것도 넣지 않습니다.
4. MdxBuilder의 포맷에는 `MDict(Html)`, Encoding엔 `UTF-8(Unicode)`를 선택합니다.
  * MdxBuilder 4.0버전에선 Sorting locale만 Korean으로 설정하면 됩니다. 
5. Start!
6. 만들어진 mdx, mdd를 MDict가 있는 기기에 넣습니다. (반드시 mdx와 mdd가 같은 경로에 있어야 합니다.)


멀티스레드 환경에서 작동하며, 2017년 3월 데이터를 기준으로 488745개의 표제어를 HTML txt 파일로 변환하는데 약 14분정도 소요됩니다. (i7-4790 기준)


## TODO

* Fix Bugs (especially Tables)
* List, Macro, etc...
* Multi-threading w/ Akka more Elegantly
* Fine commenting
* Capability of making another libraries  (e.g. making raw-text file for machine learning...)

## Issues

틀 구현 및 후처리를 위해 jQuery를 비롯한 JavaScript 파일이 포함되어 있습니다.

안드로이드/PC버전 **MDict 1.x** 버전 사용자는 **MdxBuilder 3.0**
**MDict 2.0** 버전 사용자는 **MdxBuilder 4.0**버전으로 mdx/mdd파일을 만들어야 정상 작동합니다.

일부 구형 PMP등 mdd파일(CSS/JS 포함)을 읽지 못하는 기기는 mdx only 버전을 이용해주시기 바랍니다.
(틀 미포함, CSS 인라인으로 파일 크기 커짐)

## License
Apache License 2.0
