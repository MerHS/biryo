# biryo [![Build Status](https://travis-ci.org/MerHS/biryo.svg?branch=master)](https://travis-ci.org/MerHS/biryo)

## Screenshot

![Running Demo](https://thumbs.gfycat.com/TestyCandidIsabellinewheatear-size_restricted.gif)

## Downloads

[Here](https://github.com/MerHS/biryo/releases)

## Library Dependencies

* Scala >= 2.11 (JDK >= 8)
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
  * 4.4. 줄바꿈 기준 (`WikiBlock`)
  * 4.5. 인라인 코드 (`InlineString`)
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
	* 7.2. 이미지 크기 및 위치 조절 (`htmlOption`)
* 8\. 동영상 첨부
	* 8.1. 유튜브/카카오 동영상 첨부 (`YoutubeLink`, `KakaoLink`)
	* 8.2. 니코니코 동화 동영상 첨부 (`NicoLink`)
	* 8.3. HTML5를 이용한 기타 동영상 첨부 (`HTMLBlock`)
* 9\. (Deperecated) 글상자 (`WordBox`)
* 10\. 각주 (`FootNote`, `LinkOnlyFn`)
* 11\. 들여쓰기 (`Indent`)
* 12\. 인용문 (`BlockQuote`)
* 13\. 수평줄 (`HR`)
* 14\. 주석 (`Comment`)
* 15\. 매크로
	* 15.1. date (datetime) (`DateMacro`)
	* 15.2. br (`BR`)
	* 15.3. pagecount (`PageCount`)
	* 15.4. include (`Include`)
	* 15.5. 목차 (`TableOfContents`)
	* 15.6. 각주 (`FootNoteList`)
	* 15.7. 나이 (`AgeMacro`)
	* 15.8. 남은 날 (`DDay`)
* 17\. 테이블(표) (`TableWrapper`)
	* 테이블의 각 항목은 `Table` `TR`, `TD`와 같은 HTML요소, `TableAlign`, `TableStyle`등의 CSS 요소로 구현되어 있음
* 18\. 수식 (`Math`)
* 19\. 접기 (`FoldingBlock`)
* 20\. HTML (`HTMLString`)
	* 20.1. 틀 (mdd에 저장, 원본 또한 mdx에서 볼 수 있음)
* 21\. 문법 강조 (`SyntaxBlock`)
* 22\. 분류 (`DocType`)

### Unsupported Functions / Syntax Rendering

* 자동 리다이렉트
  * MDict에 자동 리다이렉트 기능이 구현되어있으나, 앵커 사용 불가 및 디버깅 용도 등으로 인해 해당 링크를 띄우는 식으로만 구현하였습니다. 
* 이미지, 동영상 표시
  * 이미지를 비롯한 모든 파일 링크는 해당 표제어나 URL로 가는 텍스트 링크로 구현되어 있습니다. 
* 문단 접기
  * MDict 부하를 줄이기 위해 구현하지 않음
* 5.7 [], |, \, #가 제목에 쓰이거나 제목이 /로 시작하는 문서로 하이퍼링크 걸기
  * \#이 링크에 들어가면 MDict 및 MdxBuilder의 버그로 #을 %23으로 변경해도 무조건 anchor로 처리됩니다.
* 9\. 리스트
  * 들여쓰기는 구현되어 있으므로 리스트 첨자를 제외한 부분은 거의 비슷하게 보입니다.
* 15\. 매크로 중 시간과 관련된 값은 파싱한 날짜를 기준으로 처리됩니다.
* 15.3. pagecount
  * 파싱은 되지만 실제 구현은 JS로 동적으로 해야 하기 때문에 (자주 쓰이지 않는 기능이므로) 부하를 줄이기 위해 실제값으로 렌더링되지 않습니다.
* 16.4. include (틀 제외)
  * 현재 틀은 외부 JS 형식으로 변환하여 mdd에 저장하여 읽고있기 때문에 mdx-only 버전에선 해당 틀 문서로 가는 링크로 렌더링됩니다.
* 18\. 수식
    * KaTeX로 HTML을 렌더링하는 부분까진 왔지만 MDict가 커스텀 폰트 로딩이 되지 않아 렌더링이 되지 않습니다.
    * 현재는 4.4 인라인 코드와 같은 방식으로 렌더링됩니다.
* 21\. 문법 강조는 현재 4.4 인라인 코드와 같은 방식으로 렌더링됩니다.

## How to build an Executable JAR

1. [JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html) 설치
  * 환경변수 (PATH) 설정이 되어있지 않아 java 명령어가 작동하지 않으면 같이 해줍니다. 
2. [sbt](https://www.scala-sbt.org/) 설치 (Scala 빌드를 위한 툴입니다.)

다음부터는 cmd, powershell, bash등 원하는 쉘에서 

3. `git clone https://github.com/MerHS/biryo`
4. `cd biryo`
5. `sbt assembly`

target/scala-2.11 폴더에 `biryo.jar`이 만들어집니다. 


## How to Use

**주의:** 안드로이드/PC버전 **MDict 1.x** 버전 사용자는 **MdxBuilder 3.0**, 
**MDict 2.0** 버전 사용자는 **MdxBuilder 4.0**버전으로 mdx/mdd파일을 만들어야 정상 작동합니다.

mdd를 읽지 못하는 일부 기기를 위해 CSS 코드가 각 항목마다 인라인되어있는 mdx only 버전을 제작할 수 있습니다.
일반적인 경우라면 mdx, mdd를 같이 만드시면 됩니다. (mdx only버전의 경우 파일크기가 약 200MB정도 커집니다. )

위에서 jar파일을 만들었거나 받았다고 가정하고 

0. jar파일이 있는 경로에 [mdict-data](https://github.com/MerHS/biryo/tree/master/mdict-data) 폴더를 넣습니다. 스타일링 및 JS파일을 위해 반드시 필요합니다. 
1. [나무위키 데이터베이스 덤프](https://namu.wiki/w/%EB%82%98%EB%AC%B4%EC%9C%84%ED%82%A4:%EB%8D%B0%EC%9D%B4%ED%84%B0%EB%B2%A0%EC%9D%B4%EC%8A%A4%20%EB%8D%A4%ED%94%84?from=%EB%82%98%EB%AC%B4%EC%9C%84%ED%82%A4%20%EB%8D%B0%EC%9D%B4%ED%84%B0%EB%B2%A0%EC%9D%B4%EC%8A%A4%20%EB%8D%A4%ED%94%84)를 받고 압축을 풀어 jar파일과 같은 경로에 넣습니다. (이름이 namuwiki.json 파일이라고 가정)
2. `java -Dfile.encoding=UTF8 -jar biryo.jar namuwiki.json`
  * mdx only 버전용 HTML을 만들고 싶으면 `java -Dfile.encoding=UTF-8 -jar biryo.jar -inline namuwiki.json` 
  * PowerShell 등에서 `오류: 기본 클래스 .encoding=UTF-8을(를) 찾거나 로드할 수 없습니다.` 등의 에러가 뜨는 경우 `java "-Dfile.encoding=UTF8" -jar biryo.jar namuwiki.json`로 시도해보시기 바랍니다.
3. 만들어진 `namu.txt` (또는 `namu_inline.txt`)를 [MdxBuilder](https://www.mdict.cn/wp/?page_id=5325&lang=en)의 Source에, Data에는 `mdict-data`폴더의 경로를 넣고, Target에는 출력할 mdx파일의 경로 및 이름을 넣습니다.
  * mdx only 용은 Data에 아무것도 넣지 않습니다.
4. MdxBuilder의 포맷에는 `MDict(Html)`, Encoding엔 `UTF-8(Unicode)`를 선택합니다.
  * MdxBuilder 4.0버전에선 Sorting locale만 Korean으로 설정하면 됩니다. 
5. Start!
6. 만들어진 mdx, mdd를 MDict가 있는 기기에 넣습니다. (반드시 mdx와 mdd가 같은 경로에 있어야 합니다.)


멀티스레드 환경에서 작동하며, 2018년 3월 데이터를 기준으로 565985개의 표제어를 HTML txt 파일로 변환하는데 빠르면 2분정도 소요됩니다. (Ryzen 2700X, NVMe SSD 970 EVO 500GB 기준)


## Options

1. -inline: CSS 값을 link로 빼지 않고 각 문서에 인라이닝 시킵니다.
mdd 파일을 읽지 못하는 구형 MDict, PMP에서 문서를 읽을 시 이 옵션을 적용해야 합니다.

2. -thread (숫자): JSON 파서 스레드(1개) + MDict 데이터 생성 스레드(n-1개)의 개수를 조정합니다.
CPU가 4스레드 이하일시 디폴트 3, 초과시 (CPU 스레드 수 - 2) 입니다. (2는 IO 스레드용)

3. -raw: 나무마크를 파싱하지 않고 나무위키 문법이 그대로 적힌 문서를 만듭니다.


## TODO

* Fix Bugs (especially Tables)
* List, etc...
* Fine commenting
* Raw string-only Output
* Capability of making another libraries  (e.g. making raw-text file for machine learning...)

## Issues

틀 구현 및 후처리를 위해 jQuery를 비롯한 JavaScript 파일이 포함되어 있습니다.

안드로이드/PC버전 **MDict 1.x** 버전 사용자는 **MdxBuilder 3.0**
**MDict 2.0** 버전 사용자는 **MdxBuilder 4.0**버전으로 mdx/mdd파일을 만들어야 정상 작동합니다.

일부 구형 PMP등 mdd파일(CSS/JS 포함)을 읽지 못하는 기기는 mdx only 버전을 이용해주시기 바랍니다.
(틀 미포함, CSS 인라인으로 파일 크기 커짐)

## License
Apache License 2.0
