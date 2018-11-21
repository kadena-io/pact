|image0|

Pact 스마트 컨트랙트 언어 레퍼런스
==================================

본 문서는 `고성능 블록체인에 <http://kadena.io>`__\ 서의 올바른 트랜잭션
실행을 위해 설계된 스마트 컨트랙트 언어인 Pact 에 대한 레퍼런스입니다.
보다 자세한 배경 정보는
`백서 <http://kadena.io/docs/Kadena-PactWhitepaper.pdf>`__ 또 `Pact
홈페이지 <http://kadena.io/#pactModal>`__\ 를 참조하세요.

Copyright (c) 2016 - 2018, Stuart Popejoy. 무단 전재 금지.

Rest API
========

2.1.0 버전에서는 Pact 에 HTTP 서버 및 SQLite 백엔드가 기본 제공되고
있습니다. 따라서 ``pact`` 도구만으로도 블록체인 애플리케이션의
프로토타입을 제작할 수 있습니다.

서버를 시작하려면 적절한 구성과 함께 ``pact -s config.yaml``\ 를
발급합니다. pact-lang-api JS 라이브러리는 웹 개발 시 `npm 을
통해 <https://www.npmjs.com/package/pact-lang-api>`__ 사용할 수
있습니다.

.. _cmd-field-and-payloads:

``cmd`` 필드 및 페이로드
------------------------

수신된 명령이 올바른지 확인하려면 블록체인에 전송된 트랜잭션을 해시해야
합니다. 이것은 요구되는 프라이빗 키로 사인한 값이기도 합니다. 트랜잭션용
JSON 이 해시 수행에 사용된 값과 바이트 단위에서 일치하는지 확인하기 위해
JSON 을
문자열\ `“문자열화” <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify>`__\ 된의
형태로 페이로드에 *인코딩* 해야 합니다. ``cmd`` 필드는 ``exec``
페이로드와 ``cont`` 페이로드 두 가지 타입의 페이로드를 지원합니다.

.. _exec-payload:

``exec`` 페이로드
~~~~~~~~~~~~~~~~~

``exec`` 페이로드는 실행 가능한 코드와 데이터를 인코딩된 문자열 형태로
보유하고 있습니다. `send <#send>`__, `private <#private>`__, 및
`local <#local>`__ 엔드포인트는 ``cmd`` 필드에서 이 페이로드 타입을
지원합니다. 인코딩될 JSON 의 타입은 다음과 같습니다.

.. code:: javascript

    {
      "nonce": "[nonce value, needs to be unique for every call]",
      "payload": {
        "exec": {
          "code": "[pact code to be executed]",
          "data": {
            /* arbitrary user data to accompany code */
          }
        }
      }
    }

메시지를 어셈블링할 때 이 JSON 은 “문자열화”되어 ``cmd`` 필드에
제공되어야 합니다. Pact 도구의 `API 요청
포맷터 <#api-request-formatter>`__\ 의 출력을 살펴보면 ``"cmd"`` 필드를
비롯해 제공된 모든 코드가 인코딩된 JSON 의 문자열임을 확인할 수
있습니다.

.. _cont-payload:

``cont`` 페이로드
~~~~~~~~~~~~~~~~~

``cont`` 페이로드는 `pacts <#pacts>`__\ 를 진행하거나 롤백할 수 있도록
해줍니다. 이 페이로드에는 관련 Pact의 ID, Pact의 롤백 또는 진행 여부,
스텝 번호 및 필요한 모든 스텝 데이터가 포함되어 있습니다. 이러한
페이로드 필드는 다음과 같이 특별한 제약 조건을 가지고 있습니다:

-  Pact ID 는 인스턴스화된 Pact의 트랜잭션의 ID 와 일치합니다.

-  트랜잭션당 오직 하나의 Pact 만 인스턴스화할 수 있습니다.

-  Pact 가 롤백 중인 경우에는 스텝 번호가 방금 실행된 스텝과 일치해야
   합니다.

-  Pact 가 진행 중인 경우에는 스텝 번호가 방금 실행된 하나 이상의 스텝과
   일치해야 합니다.

``exec`` 페이로드 필드에서와 마찬가지로 ``cont`` 페이로드 필드 역시
문자열 형태로 인코딩 해야 합니다. `send <#send>`__ 엔드포인트는 ``cmd``
필드에서 이 페이로드 타입을 지원합니다. 인코딩되는 JSON 의 포맷은 다음과
같습니다.

.. code:: javascript

    {
      "nonce": "[nonce value, needs to be unique for every call]",
      "payload": {
        "cont": {
          "txid": [transaction id where pact instantiated]
          "rollback": [true or false],
          "step": [step to be continued or rolled back, needs to be integer between 0 and (total number of steps - 1)]
          "data": {
            /* arbitrary user data to accompany step code */
          }
        }
      }
    }

엔드포인트
----------

모든 엔드포인트는 ``api/v1``\ 에서 지원됩니다. 따라서
``localhost:8080``\ 에서 실행 중인 경우 ``send`` 호출은
http://localhost:8080/api/v1/send\ 로 전송됩니다.

/send
~~~~~

하나 이상의 *퍼블릭* (암호화되지 않은) 명령이 블록체인에 비동기식으로
제출됩니다. `cmd 필드 및 페이로드 <#cmd-field-and-payloads>`__\ 을
참조하세요.

요청 JSON:

.. code:: javascript

    {
      "cmds": [
      {
        "hash": "[blake2 hash in base16 of 'cmd' string value]",
        "sigs": [
          {
            "sig": "[crypto signature by secret key of 'hash' value]",
            "pubKey": "[base16-format of public key of signing keypair]",
            "scheme": "ED25519" /* optional field, defaults to ED25519, will support other curves as needed */
          }
        ]
        "cmd": "[stringified transaction JSON]"
      }
      // ... more commands
      ]
    }

응답 JSON:

::

    {
      "status": "success|failure",
      "response": {
        "requestKeys": [
          "[matches hash from each sent/processed command, use with /poll or /listen to get tx results]"
        ]
      }
    }

/private
~~~~~~~~

하나 이상의 *프라이빗* 명령이 블록체인에 비동기식으로 제출됩니다 (전송
및 수신 엔터티만 읽을 수 있도록 안전하게 암호화하기 위해 제공된 주소
정보를 사용). 문자열화된 JSON 데이터에 관한 내용은 `cmd 필드 및
페이로드 <#cmd-field-and-payloads>`__\ 을 참조하세요.

요청 JSON:

.. code:: javascript

    {
      "cmds": [
      {
        "hash": "[blake2 hash in base16 of 'cmd' string value]",
        "sigs": [
          {
            "sig": "[crypto signature by secret key of 'hash' value]",
            "pubKey": "[base16-format of public key of signing keypair]",
            "scheme": "ED25519" /* optional field, defaults to ED25519, will support other curves as needed */
          }
        ]
        "cmd": "[stringified transaction JSON]"
      }
      ]
    }

응답 JSON:

::

    {
      "status": "success|failure",
      "response": {
        "requestKeys": [
          "[matches hash from each sent/processed command, use with /poll or /listen to get tx results]"
        ]
      }
    }

/poll
~~~~~

명령 결과에 대해 투표를 합니다.

요청 JSON:

::

    {
      "requestKeys": [
        "[hash from desired commands to poll]"
      ]
    }

응답 JSON:

::

    {
      "status": "success|failure",
      "response": {
        "[command hash]": {
          "result": {
            "status": "success|failure",
            "data": /* data from Pact execution represented as JSON */
          },
          "txId": /* integer transaction id, for use in querying history etc */
        }
      }
    }

/listen
~~~~~~~

단일 명령 결과를 수신하거나 이미 실행된 명령을 검색하기 위한 블로킹
호출입니다.

요청 JSON:

::

    {
      "listen": "[command hash]"
    }

응답 JSON:

::

    {
      "status": "success|failure",
      "response": {
        "result": {
          "status": "success|failure",
          "data": /* data from Pact execution represented as JSON */
        },
        "txId": /* integer transaction id, for use in querying history etc */
      }
    }

/local
~~~~~~

비 트랜잭션 (non-transactional) 실행을 위해 명령을 전송하는
블로킹/동기화 호출입니다. 블록체인 환경에서 이 호출은 노드 로컬 방식으로
“더티 리드(dirty read)”됩니다. 환경에 대한 모든 데이터베이스 쓰기/변경
사항이 롤백됩니다. 문자열화된 JSON 데이터에 관한 내용은 `cmd 필드 및
페이로드 <#cmd-field-and-payloads>`__\ 을 참조하세요.

요청 JSON:

::

    {
      "hash": "[blake2 hash in base16 of 'cmd' value]",
      "sigs": [
        {
          "sig": "[crypto signature by secret key of 'hash' value]",
          "pubKey": "[base16-format of public key of signing keypair]",
          "scheme": "ED25519" /* optional field, defaults to ED25519, will support other curves as needed */
        }
      ]
      "cmd": "[stringified transaction JSON]"
    }

응답 JSON:

::

    {
      "status": "success|failure",
      "response": {
        "status": "success|failure",
        "data": /* data from Pact execution represented as JSON */
      }
    }

API 요청 포맷터
---------------

Pact 2.2.3 에서는 ``pact`` 도구가 ``-a`` 옵션을 수락하여 해당 요청을
설명하는 YAML 파일을 사용해 API 요청 JSON 을 포맷합니다. 출력된 정보는
Postman 혹은 ``curl``\ 과 같은 POST 도구와 사용할 수 있습니다.

다음과 콘텐츠가 포함된 “apireq.yaml”라는 yaml 파일을 예로 들 수
있습니다.

::

    code: "(+ 1 2)"
    data:
      name: Stuart
      language: Pact
    keyPairs:
      - public: ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d
        secret: 8693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17332

를 ``pact``\ 에 제공해서 유효한 API 요청을 얻을 수 있습니다.

::

    $ pact -a tests/apireq.yaml -l
    {"hash":"444669038ea7811b90934f3d65574ef35c82d5c79cedd26d0931fddf837cccd2c9cf19392bf62c485f33535983f5e04c3e1a06b6b49e045c5160a637db8d7331","sigs":[{"sig":"9097304baed4c419002c6b9690972e1303ac86d14dc59919bf36c785d008f4ad7efa3352ac2b8a47d0b688fe2909dbf392dd162457c4837bc4dc92f2f61fd20d","scheme":"ED25519","pubKey":"ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d"}],"cmd":"{\"address\":null,\"payload\":{\"exec\":{\"data\":{\"name\":\"Stuart\",\"language\":\"Pact\"},\"code\":\"(+ 1 2)\"}},\"nonce\":\"\\\"2017-09-27 19:42:06.696533 UTC\\\"\"}"}

아래 코드는 포트 8080 에서 실행 중인 Pact 서버에 일치하는 curl 으로
파이핑을 하는 예입니다.

::

    $ pact -a tests/apireq.yaml -l | curl -d @- http://localhost:8080/api/v1/local
    {"status":"success","response":{"status":"success","data":3}}

.. _request-yaml:

요청 YAML 파일 포맷
~~~~~~~~~~~~~~~~~~~

요청 YAML 파일에는 두 가지 타입이 있습니다. *실행* 타입의 요청 YAML
파일은 `exec <#exec-payload>`__ 페이로드를 설명합니다. 반면 *진행*
타입의 요청 YAML 파일은 `cont <#cont-payload>`__ 페이로드를 설명합니다.

실행 타입의 요청 YAML 파일은 다음 키를 가져옵니다.

.. code:: yaml

      code: Transaction code
      codeFile: Transaction code file
      data: JSON transaction data
      dataFile: JSON transaction data file
      keyPairs: list of key pairs for signing (use pact -g to generate): [
        public: base 16 public key
        secret: base 16 secret key
        ]
      nonce: optional request nonce, will use current time if not provided
      from: entity name for addressing private messages
      to: entity names for addressing private messages

진행 타입의 요청 YAML 파일은 다음 키를 가져옵니다.

.. code:: yaml

      type: "cont"
      txId: Integer transaction id of pact
      step: Integer next step of a pact
      rollback: Boolean for rollingback a pact
      data: JSON transaction data
      dataFile: JSON transaction data file
      keyPairs: list of key pairs for signing (use pact -g to generate): [
        public: base 16 public key
        secret: base 16 secret key
        ]
      nonce: optional request nonce, will use current time if not provided
      from: entity name for addressing private messages
      to: entity names for addressing private messages

.. _concepts:

개념
====

.. _execmodes:

실행 모드
---------

Pact는 블록체인에서 고속 선형 실행의 성능 요구 사항을 충족하도록 서로
다른 *실행 모드* 에서 사용할 수 있도록 설계되었습니다. 요구 사항은
다음과 같습니다.

1. 컨트랙트 정의.
2. 트랜잭션 실행.
3. 쿼리 및 로컬 실행.

.. _definitionmode:

컨트랙트 정의
~~~~~~~~~~~~~

해당 모드에서는 코드(모듈), 테이블(데이터) 및 키셋(인증)으로 구성된
스마트 컨트랙트을 설정하기 위해 대량의 코드가 블록체인으로 전송됩니다.
데이터 초기화가 가능한 “트랜잭셔널”(데이터베이스를 수정하는) 코드도
포함될 수 있습니다.

이러한 코드는 단일 메시지 형태로 블록체인에 전송해야 어떠한 오류라도
하나의 단위로 전체 스마트 컨트랙트를 롤백시키게 됩니다.

.. _keysetdefinition:

키셋 정의
^^^^^^^^^

`키셋 <#confidential-keysets>`__\ 은 모듈 및 테이블에서 관리 권한 부여
체계를 지정하기 위해 사용되기 때문에 보통은 가장 먼저 정의됩니다. 정의
작업을 통해 런타임 환경에서 키셋을 생성하고 전역 키셋 데이터베이스에
이러한 정의를 저장할 수 있습니다.

.. _moduledeclaration:

모듈 선언
^^^^^^^^^

`모듈 <#module>`__\ 에는 스마트 컨트랙트을 위한 API 및 데이터 정의가
포함되어 있습니다. 모듈의 구성 요소는 다음과 같습니다.

-  `함수 <#defun>`__
-  `스키마 <#defschema>`__ 정의
-  `테이블 <#deftable>`__ 정의
-  `“pact” <#defpact>`__ 특별 함수
-  `const(상수) <#defconst>`__ 값

모듈이 선언될 때 다른 모듈에서의 기본 함수 또는 정의에 대한 모든
레퍼런스가 귀결됩니다. 귀결 실패 시 트랜잭션 롤백으로 이어집니다.

관리자 키셋으로 제어가 되도록 모듈을 재정의할 수 있습니다. 모듈 이름에
버전 sigil 을 포함시키는 경우(예: “accounts-v1”)를 제외하고는 모듈 버전
관리가 지원되지 않습니다. *모듈 해시* 는 코드 안전성을 보장하기 위한
강력한 기능입니다. `use <#use>`__ 통해 모듈을 가져올 때 특정 릴리스에
코드를 연결하도록 모듈 해시를 지정할 수 있습니다.

Pact 2.2 에서는 모듈 선언 내에 ``use`` 문을 발행할 수 있습니다. 모듈
해시와 이 기능을 결합해 사용하면 종속 모듈이 해당 체인에서 이후에 변경된
경우에 업데이트된 모듈 코드를 가져올 수 없도록 하는 등 높은 수준의
보증을 제공할 수 있습니다. 또한 로드된 모듈의 해시에 대한 변경 사항을
전파시켜서 부주의로 인한 업데이트 변경으로부터 다운스트림 모듈을 보호할
수 있습니다.

모듈 이름은 전역적으로 고유해야 합니다

.. _tablecreation:

테이블 생성
^^^^^^^^^^^

테이블은 모듈과 동시에
`생성 <pact-functions.html#create-table>`__\ 됩니다. 테이블은 모듈에서
*정의* 가 되지만 모듈보다 “나중에” *생성* 되기 때문에 테이블을 반드시
재생성하지 않고도 모듈을 재정의할 수 있습니다.

모듈과 테이블의 관계는 중요합니다 `테이블 보호
참조 <#module-table-guards>`__.

생성할 수 있는 테이블의 수에는 제한이 없습니다. 테이블 이름이 모듈
이름으로 네임스페이스됩니다.

`스키마 <#defschema>`__\ 의 타입을 가질 수 있습니다.

.. _transaction-execution:

트랜잭션 실행
~~~~~~~~~~~~~

“트랜잭션”이란 결제, 판매, 복잡한 컨트랙트의 워크플로우 스텝와 같이
블록체인에서 수행되는 비즈니스 이벤트를 뜻합니다. 일반적으로 트랜잭션은
모듈 함수에 대한 단일 호출입니다. 하지만 실행할 수 있는 명령문의 수에는
제한이 없습니다. 실제로 “트랜잭션”과 “스마트 컨트랙트 정의”는 실행되는
코드의 *종류* 에만 차이가 있고, 코드 평가에서는 실질적인 차이가
없습니다.

.. _queries:

쿼리 및 로컬 실행
~~~~~~~~~~~~~~~~~

데이터 쿼리는 보통 비즈니스 이벤트가 아니며 성능에 영향을 미칠 수 있는
데이터 페이로드가 포함될 수 있기 때문에 쿼리는 메시지를 수신하는
노드에서 *로컬 실행* 형태로 수행됩니다. 이력 쿼리는 *transaction ID* 를
기준점으로 사용하여 경쟁 상태를 방지하고 비동기식 쿼리 실행을
지원합니다.

서로 다른 API 엔드 포인트를 대상으로 트랜잭션 실행과 로컬 실행에 대한
비교가 이루어집니다. Pact 코드는 트랜잭셔널 실행과 로컬 실행을 구분할 수
없습니다.

.. _dbinteraction:

데이터베이스 상호 작용
----------------------

Pact는 다른 백엔드에서 실행되도록 변경이 가능한 블록체인 실행 고유의
요구 사항을 반영하여 데이터베이스 메타포를 제공합니다.

.. _atomicexecution:

원자적 실행 (Atomic execution)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

블록체인에 전송되고 Pact 가 평가하는 단일 메시지는 *원자적(atomic)*
입니다. 트랜잭션은 하나의 단위로 성공하거나 아예 성공하지
않습니다(데이터베이스 관련 자료에서 “트랜잭션”이라고 함). 롤백 처리는
명시적으로 지원되지 않습니다 (`멀티스텝 <#pacts>`__ 트랜잭션은 예외).

.. _dbkeyrow:

Key-Row 모델
~~~~~~~~~~~~

블록체인 실행은 단일 테이블에 기록된 역정규화 데이터를 선호하는 온라인
트랜잭션 처리(OLTP) 데이터베이스 워크로드로 비유할 수 있습니다. Pact의
데이터 액세스 API 는 이를 반영해 *key-row* 모델을 제공하고 있는데, 이
모델에서는 열 값에 대한 행을 단일 키로 액세스할 수 있습니다.

그 결과 Pact는 *조인* 테이블을 지원하지 않기 때문에 Pact
데이터베이스에서 가져온 내용이 채워지는 온라인 분석 처리(OLAP)
데이터베이스에 더 적합합니다. 그렇다고 해서 Pact 가 관계 기법을 사용해
트랜잭션을 *기록* 할 수 없는 것은 아닙니다. 예를 들어 판매 테이블에서
사용되는 키를 가진 고객 테이블에서는 판매 테이블에 쓰기를 수행하기 전에
코드가 고객 레코드를 조회합니다.

.. _queryperformance:

쿼리 및 성능
~~~~~~~~~~~~

Pact 2.3 에서는 Pact 가 테이블에서 여러 행을 선택할 수 있도록 강력한
쿼리 메커니즘을 제공합니다. SQL 과 시각적으로는 유사하지만
`select <pact-functions.html#select>`__ 및
`where <pact-functions.html#where>`__ 연산이 테이블에 *스트리밍
인터페이스* 를 제공합니다. 테이블에서는 사용자가 필터 함수를 제공한
다음, `sort <pact-functions.html#sort>`__ 및 기타 함수를 사용하여 리스트
데이터 구조로서 행 집합을 사용합니다.

.. code:: lisp

    ;; the following selects Programmers with salaries >= 90000 and sorts by age descending

    (reverse (sort ['age]
      (select 'employees ['first-name,'last-name,'age]
        (and? (where 'title (= "Programmer"))
              (where 'salary (< 90000))))))

    ;; the same query could be performed on a list with 'filter':

    (reverse (sort ['age]
      (filter (and? (where 'title (= "Programmer"))
                    (where 'salary (< 90000)))
              employees)))

트랜잭셔널 설정에서 Pact 데이터베이스 상호 작용은 단일 행 읽기 및 쓰기
작업에 대해 최적화되기 때문에 이러한 쿼리의 성능이 저하되고 컴퓨팅
비용이 엄청나게 많이 들 수 있습니다. 하지만 Pact는 `로컬 <#local>`__
실행 기능을 사용하여 스트리밍 결과에 대해 사용자 필터 함수를
활용함으로써 뛰어난 성능을 제공할 수 있습니다.

따라서 가장 좋은 방법은 비 트랜잭션 로컬 연산을 통해 엄선된 연산을
사용하고, 트랜잭셔널 설정 시 대형 테이블에서 이렇게 선택한 연산을
사용하지 않는 것입니다.

.. _nonulls:

NULL 값 없음
~~~~~~~~~~~~

Pact의 경우 데이터베이스 메타포에서 NULL 값이라는 개념이 없습니다. Row
값이 발견되지 않으면 데이터베이스 결과에 대한 컴퓨팅을 위한 메인 함수인
`with-read <pact-functions.html#with-read>`__\ 에 오류가 발생합니다.
작성자는 모든 트랜잭션 읽기 작업에 대한 값이 존재하는지 확인해야 합니다.
이는 *전체성* 을 보장하고 NULL 값을 둘러싼 불필요하고 안전하지 않은 제어
흐름을 피하기 위한 보안 기능입니다.

.. _dbversions:

버전 관리 이력
~~~~~~~~~~~~~~

Key-Row 모델은 트랜잭션 ID 로 버전 관리가 되고 있는 열 값을 모두
변경하는 방법으로 보강되었습니다. 예를 들어 “이름”, “연령”, “역할” 등 3
개의 열을 가진 테이블은 트랜잭션 #1 의 “이름”과 트랜잭션 2 의 “연령”이
업데이트됩니다. 이력 데이터를 검색하면 트랜잭션 #1 의 “이름”에 대한 변경
사항과 트랜잭션 #2 의 “연령” 및 “역할”에 대한 변경 사항이 반환됩니다.

.. _backends:

백엔드
~~~~~~

Pact는 블록체인 내의 스마트 컨트랙트 레이어에서 동일하고 올바른 실행을
보장합니다. 그 결과, 서로 다른 합의 노드에서 백킹 스토어가 동일할 필요가
없습니다. Pact를 구현하면 산업용 RDBMS 를 통합하여 데이터를 다운스트림
시스템으로 손쉽게 대량 복제함으로써 블록체인 기반 시스템으로 대대적인
마이그레이션을 수행할 수 있습니다.

타입 및 스키마
--------------

Pact 2.0 에서는 Pact 가 비록 선택 사항이기는 하지만 명시적 타입의 사양을
얻을 수 있습니다. 타입이 없는 Pact 1.0 코드는 이전처럼 기능하며, 타입이
없는 쓰기 코드는 신속한 프로토타입 제작에 유리합니다.

스키마는 타입이 필요한 큰 이유입니다. 타입이 있는(필수는 아님) 열
리스트를 통해 스키마가 `정의됩니다 <#defschema>`__. 그런 다음, 특정
스키마를 통해 테이블이 `정의됩니다 <#deftable>`__.

스키마는 객체 타입에서도 사용 또는 지정이 가능합니다.

런타임 Type Enforcement (TE)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

코드에서 선언된 모든 타입은 런타임 시 적용됩니다. 따라서 테이블
스키마에서 테이블에 대한 모든 쓰기 작업은 스키마를 토대로
타입체킹됩니다. 그렇지 않고 타입 사양이 나타나는 경우에는 런타임 시
표현이 평가될 때 타입이 적용됩니다.

모듈에서의 정적 타입 추론 (Static Type Inference)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pact 해석기는 `typecheck <pact-functions.html#typecheck>`__ REPL 명령을
통해 모듈을 분석하고 모든 변수, 함수 애플리케이션 또는 상수 정의에 있어
타입 추론을 시도합니다. 프로젝트 REPL 스크립트에서 이 명령을 사용하면
개발자가 타입체킹을 성공적으로 할 수 있기 때문에 “충분한 타입들”을
추가할 수 있습니다. 타입체킹을 완벽하게 성공하기 위해서는 모든 테이블에
스키마를 제공하고 애매하거나 과부하 상태의 기본 함수를 호출하는 보조
함수에 인수 타입을 제공해야 합니다.

정형 검증 (Formal Verification)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pact의 타입체킹 도구는 SMT-LIB2 에서 정형 증명을 생성하는데 사용하기
위해 완벽하게 타입체킹이 이루어지고 인라이닝된 AST 를 출력하도록
설계되었습니다. 타입체킹이 완벽하게 성공하지 않으면 모듈은 “증명 가능한”
상태로 간주되지 않습니다.

따라서 Pact 코드는 처음에는 타입 없이, 그런 다음에는 “충분한” 타입으로,
그리고 마지막에는 정형 증명으로 “안전성”을 높여나갈 수 있습니다.

Pact 2.0 에서는 정형 검증 함수가 아직 개발 중입니다.

.. _confidential-keysets:

키셋 및 인증
------------

Pact는 비트코인 스크립트의 영향을 받아 스마트 컨트랙트 실행 및 관리에
직접 퍼블릭 키 인증을 통합했습니다.

키셋 정의
~~~~~~~~~

키셋은 메시지 페이로드에서 정의를
`읽어 <pact-functions.html#read-keyset>`__
`정의됩니다 <pact-functions.html#define-keyset>`__.키셋은 퍼블릭 키
리스트와 *키셋 predicate* 으로 이루어져 있습니다.

유효한 키셋 JSON 제작 예시:

.. code:: javascript

    /* examples of valid keysets */
    {
      "fully-specified-with-native-pred":
        { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"], "pred": "keys-2" },

      "fully-specified-with-qual-custom":
        { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"], "pred": "my-module.custom-pred" },

      "keysonly":
        { "keys": ["abc6bab9b88e08d","fe04ddd404feac2"] }, /* defaults to "keys-all" pred */

      "keylist": ["abc6bab9b88e08d","fe04ddd404feac2"] /* makes a "keys-all" pred keyset */
    }

.. _keyset-predicates:

키셋 predicate
~~~~~~~~~~~~~~

키셋 predicate 은 (선택적으로 자격이 있는) 이름으로 함수를 참조하는데,
이러한 이름은 키셋의 퍼블릭 키를 블록체인 메시지를 사인하는 데 사용되는
키(들)와 비교합니다. 이 함수는 “count”와 “matched”라는 두 개의 인수를
허용하는데, “count”는 키셋의 키 개수이고 “matched”는 키셋의 키와
일치하는 메시지 시그니처의 키 개수를 뜻합니다.

다중 시그니처 지원은 블록체인 계층의 책임으로, 비트코인 스타일의 “다중
시그니처” 컨트랙트 (펀드 퍼블릭에 최소 두 개의 시그니처가 필요)의 강력한
기능입니다.

Pact 에는 `keys-all <pact-functions.html#keys-all>`__,
`keys-any <pact-functions.html#keys-any>`__,
`keys-2 <pact-functions.html#keys-2>`__ 같은 키셋 predicate 이
기본적으로 포함되어 있습니다. 모듈 작성자는 자유롭게 추가 predicate 을
정의할 수 있습니다.

키셋 predicate 이 지정되어 있지 않은 경우에는
`keys-all <pact-functions.html#keys-all>`__\ 으로 기본 설정됩니다.

.. _keyrotation:

키 순환(Rotation)
~~~~~~~~~~~~~~~~~

키셋은 순환(rotate) 될 수 있지만, 현재 키셋 정의 및 predicate 을
기준으로 권한이 부여된 메시지에 의해서만 가능합니다. 권한이 부여된
키셋은 손쉽게 `재정의 <#define-keyset>`__\ 가 가능합니다.

.. _tableguards:

모듈 테이블 보호
~~~~~~~~~~~~~~~~

테이블을 `생성 <pact-functions.html#create-table>`__\ 할 때 모듈 이름도
반드시 지정해야 합니다. 이 메커니즘에서는 `data-access
함수 <pact-functions.html#database>`__\ 를 통한 테이블 직접 액세스가
모듈의 관리자 키셋에 의해 권한이 부여되도록 모듈별로 테이블이 “보호”
또는 “캡슐화” 됩니다. 한편 *모듈 함수 내* 에서는 테이블 액세스가
자유롭습니다. 이 기능은 컨트랙트서 작성자가 데이터 액세스를 자유롭게
설계하도록 도와주고, 모듈을 주 “사용자” 데이터 액세스 API 로 소중하게
다룰 수 있게 해줍니다.

.. _rowlevelkeysets:

Row-level 키셋
~~~~~~~~~~~~~~

키셋은 하나의 행에 하나의 열 값으로 저장이 가능하기 때문에 *row-level*
승인이 가능합니다. 다음 코드는 이를 달성할 수 있는 방법을 보여줍니다

.. code:: lisp

    (defun create-account (id)
      (insert accounts id { "balance": 0.0, "keyset": (read-keyset "owner-keyset") }))

    (defun read-balance (id)
      (with-read accounts id { "balance":= bal, "keyset":= ks }
        (enforce-keyset ks)
        (format "Your balance is {}" [bal])))

이 예시에서 ``create-account``\ 는 테이블에 “키셋” 형태로 저장할 수
있도록 `read-keyset <pact-functions.html#read-keyset>`__\ 을 사용해
메시지 페이로드로부터 키셋 정의를 읽어들입니다. ``read-balance``\ 는
먼저 `enforce-keyset <pact-functions.html#enforce-keyset>`__\ 을 사용해
키셋을 적용함으로써 해당 소유자의 키셋은 나머지만 읽을 수 있도록
허용합니다.

.. _computation:

계산 모델
---------

여기에서는 Pact의 계산 접근 방식을 다양한 측면에서 살펴봅니다.

.. _turingincomplete:

튜링 불완전성 (Turing-Incomplete)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pact는 튜링 불완전성을 보이기 때문에 재귀가 발생하지 않고(재귀는 실행
전에 감지되고 오류를 발생) 무한대 루프가 불가능합니다. Pact는
`맵(map) <pact-functions.html#map>`__,
`폴드(fold) <pact-functions.html#fold>`__ 및
`필터(filter) <pact-functions.html#filter>`__\ 를 통해 리스트 구조에
대한 연산을 지원하지만, 무한대 리스트를 정의할 수 없다는 점에서 반드시
유계되는 것은 아닙니다.

튜링 불완전성 덕분에 Pact 모듈 로딩 시 모든 참조를 미리 결정할 수
있으며, 이는 곧 조회 테이블에서 함수를 처리하는 대신에 함수 정의를 콜
사이트에 직접 주입(또는 “인라이닝”)할 수 있다는 것을 의미합니다. 이는
튜링 불완전성을 나타내는 언어의 성능 이점을 보여주는 예입니다.

.. _variables:

단일 할당 변수 (Single-assignment Variables)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pact에서는 `let 표현식 <#let>`__ 및 `바인딩 <#bindings>`__\ 에서 변수
선언이 가능합니다. 변수는 변경이 불가능하기 때문에 현장에서 재할당 또는
수정을 할 수 없습니다.

일반적인 변수 선언은 `with-read <pact-functions.html#with-read>`__
함수에서 이루어지며 이름에 따라 열 값에 변수를 할당합니다.
`bind <pact-functions.html#bind>`__ 함수는 객체에 이와 동일한 기능을
제공합니다.

모듈 전역 상수 값은 `defconst <#defconst>`__\ 를 통해 선언이 가능합니다.

.. _datatypes:

데이터 타입
~~~~~~~~~~~

Pact 코드는 명시적인 타입을 가질 수 있고, 문서화된 타입 시그니처 언어에
표시된 대로 기본 함수가 엄격한 유형 검사를 수행하기 때문에 보이지 않는
곳에서도 항상 강력한 타입을 갖습니다.

Pact 에서 지원하는 타입은 다음과 같습니다.

-  `문자열 <#strings>`__
-  `정수 <#integers>`__
-  `10진수 <#decimals>`__
-  `부울 <#booleans>`__
-  `키셋 <#confidential-keysets>`__
-  `리스트 <#lists>`__
-  `객체 <#objects>`__
-  `함수 <#defun>`__ 및 `pact <#defpact>`__ 정의
-  `JSON 값 <#json>`__
-  `테이블 <#deftable>`__
-  `스키마 <#defschema>`__

.. _performance:

성능
~~~~

Pact는 블록체인에 비즈니스 이벤트를 신속하게 기록하기 유리하도록 쿼리 및
모듈 정의에 페널티를 부과하여 `트랜잭션
실행 <#transaction-execution>`__\ 의 성능을 극대화 하도록
디자인되었습니다. 신속한 실행을 위한 몇 가지 팁은 다음과 같습니다.

.. _singlefunctiontx:

단일 함수 트랜잭션 (Single-function transactions)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

단일 함수 호출을 통해 실행이 가능하도록 트랜잭션을 설계합니다.

.. _usereferences:

``use`` 대신 레퍼런스로 호출합니다.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

트랜잭션 시 모듈 함수를 호출할 때 `use <#use>`__ 함수를 통해 모듈을
가져오는 대신에 `레퍼런스 문법 <#references>`__\ 을 사용합니다. 이러한
레퍼런스들이 모듈 정의 시간에 인라이닝되므로 다른 모듈 함수를 참조하는
모듈을 정의할 때는 ``use``\ 가 좋습니다.

.. _argsvmsgs:

하드 코딩된 인수 vs 메시지 값
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

트랜잭션은 트랜잭션 코드로 값을 직접 인코딩할 수 있습니다.

::

    (accounts.transfer "Acct1" "Acct2" 100.00)

또는 메시지 JSON 페이로드에서 값을 읽어올 수 있습니다.

::

    (defun transfer-msg ()
      (transfer (read-msg "from") (read-msg "to")
                (read-decimal "amount")))
    ...
    (accounts.transfer-msg)

후자의 경우, 트랜잭션 시 해석할 코드 수가 적으므로 실행 속도가 약간 더
빠릅니다.

필요 시 타입 추가
^^^^^^^^^^^^^^^^^

테이블 스키마 덕분에 Pact는 거의 모든 사용 사례에서 강력한 타입을 갖게
되지만, 데이터베이스를 사용하지 않는 함수에서도 여전히 타입이
필요합니다. `typecheck <typecheck>`__ REPL 함수를 사용하여 필요한 타입을
추가합니다. 런타임 시 Type Enforcement(TE)에 드는 비용이 적지만, 너무
많은 수의 타입 시그니처로 인해 가독성이 저하될 수 있습니다. 그러나
타입은 API 를 문서화하는 데 도움이 되기 때문에 타입 사용은 판단에 따라
결정해야 합니다.

.. _controlflow:

Control Flow
~~~~~~~~~~~~

Pact는 `if <pact-functions.html#if>`__, 바운드 루핑 및 함수
애플리케이션을 통해 조건문을 지원합니다.

.. _evilif:

“If”는 유해한 것으로 간주
^^^^^^^^^^^^^^^^^^^^^^^^^

가능하면 ``if`` 사용은 피하세요. 모든 분기에서 이해하기 어렵고 버그에
취약한 코드가 만들어지고 있기 때문입니다. 프론트 엔드에는 “지금 어떤
무엇을 하려 하는가”에 대한 코드를, 스마트 컨트랙트에는 “지금 하려는
트랜잭션이 올바른지 검증”을 하는 코드를 집어 넣는 것이 가장 좋은
방법입니다.

Pact는 원래 ``if``\ (및 루핑)를 일체 사용하지 않도록 설계되었지만,
사용자가 필요 시에는 이러한 기능을 현명하게 사용할 수 있도록 해야 한다고
판단했습니다.

.. _use-the-enforce-luke:

enforce 사용
^^^^^^^^^^^^

“if”는 비즈니스 로직 불변식을 적용하는 데 사용해서는 안 됩니다. 대신에
`enforce <pact-functions.html#enforce>`__\ 를 선택해서 트랜잭션을
실패시키는 것이 좋습니다

실제로 실패는 Pact 에서 허용되는 유일한 *비 로컬 종료* 입니다. 이것을
보더라도 Pact 가 *전체성* 에 중점을 두고 있다는 것을 알 수 있습니다.

`enforce-one <pact-functions.html#enforce-one>`__\ (Pact 2.3 에 새롭게
추가)을 사용하면 하나가 통과되면 전체 표현식이 통과되도록 적용 리스트를
테스트할 수 있습니다. 이는 Pact 에서 “예외 포착”을 보여주는 유일한
예입니다. 예외 포착의 경우 적용이 실패하면 다음 테스트가 실행되고
테스트를 통과하면 쇼트 서킷이 이루어집니다.

내장된 키셋 사용
^^^^^^^^^^^^^^^^

`keys-all <pact-functions.html#keys-all>`__,
`keys-any <pact-functions.html#keys-any>`__,
`keys-2 <pact-functions.html#keys-2>`__ 같이 내장된 키셋 함수는 신속한
실행을 위해 해석기에 하드 코딩됩니다. 사용자 지정 키셋의 경우 런타임 시
결정이 필요하기 때문에 속도가 저하됩니다.

.. _fp:

함수형 개념
~~~~~~~~~~~

Pact 에는 `맵(map) <pact-functions.html#map>`__,
`폴드(fold) <pact-functions.html#fold>`__ 및
`필터(filter) <pact-functions.html#filter>`__\ 와 같은 함수형 프로그래밍
“최고 인기 기법”이 포함되어 있습니다. 이들은 모두 `부분
애플리케이션 <#partial-application>`__\ 을 채택하고 있어서 함수를
연속적으로 실행하도록 리스트 항목이 애플리케이션 인수에 추가됩니다.

.. code:: lisp

    (map (+ 2) [1 2 3])
    (fold (+) "" ["Concatenate" " " "me"])

또한 Pact는 `compose <pact-functions.html#compose>`__\ 를 가지고 있어서
함수형 스타일으로 애플리케이션을 “체인 연결”할 수 있습니다.

.. _pure:

순수한(Pure) 실행
~~~~~~~~~~~~~~~~~

특정 맥락에서 Pact는 계산이 “순수하게”(pure) 이루어지도록 보장할 수
있습니다. 여기에서 순수하다는 것은 데이터베이스 상태를 액세스 또는
수정하지 않는다는 의미입니다. 현재 ``enforce``, ``enforce-one`` 및 키셋
Predicate 평가는 모두 순수한 맥락에서 실행되고 있습니다.
`defconst <#defconst>`__ 메모이제이션 역시 순수하게 이루어집니다.

LISP
~~~~

Pact 가 LISP 구문을 사용하는 목적은 런타임 표현을 직접 코드에 반영하여
컨트랙트서 작성자가 프로그램 실행에 직접 초점을 맞추도록 하기 위한
것입니다. Pact 코드는 원장에 사람이 읽을 수 있는 형태로 저장되므로
코드를 직접 확인하는 것이 가능하며, LISP 스타일의 `s-표현식 <#sexp>`__
구문을 사용하면 이 코드를 신속하게 실행할 수 있습니다.

.. _messagedata:

메시지 데이터
~~~~~~~~~~~~~

Pact에서는 JSON 페이로드 및 시그니처를 통해 메시지에 코드가 도달합니다.
메시지 데이터는 `read-msg <pact-functions.html#read-msg>`__ 및 관련
함수를 사용해 읽는 반면, 시그니처는 직접 읽기 또는 쓰기가 불가능합니다.
따라서 이들은 `키셋 Predicate <#keysetpredicates>`__ 적용 과정에서
평가됩니다.

.. _json:

JSON 지원
^^^^^^^^^

Pact 트랜잭션에서 반환된 값들은 JSON 값으로 직접 표현됩니다.

`read-msg <pact-functions.html#read-msg>`__\ 를 통해 메시지에서 값을
읽을 때 Pact는 다음과 같이 JSON 타입을 강제 변환합니다.

-  문자열 -> 문자열
-  숫자 -> 정수(반올림)
-  부울 -> 부울
-  객체 -> 객체
-  array -> list
-  Null -> JSON 값

10진수는 문자열로 표현되고
`read-decimal <pact-functions.html#read-decimal>`__\ 을 사용해
읽어들입니다.

.. _confidentiality:

기밀 유지
---------

Pact는 *기밀성이 유지되는* 환경에서 사용이 되도록 설계되었기 때문에
참가자의 일부만 메시지를 볼 수 있습니다. 이러한 기밀성은 스마트 컨트랙트
실행에 큰 영향을 미칩니다.

엔터티(Entities)
~~~~~~~~~~~~~~~~

*엔터티(Entity)* 는 기밀 메시지를 볼 수 있거나 없는 비즈니스
참가자입니다. 엔터티는 기업, 기업 내 그룹 또는 개인이 될 수 있습니다.

.. _disjointdbs:

분리 데이터베이스
~~~~~~~~~~~~~~~~~

Pact의 스마트 컨트랙트은 블록체인로 구성된 메시지에서 수행되며, 트랜잭션
실행의 결과가 포함된 레코드의 데이터베이스를 만드는 데 도움이 됩니다.
기밀 환경에서는 엔터티마다 서로 다른 트랜잭션을 수행하기 때문에 그
결과로 생성되는 데이터베이스가 *분리* 됩니다.

이러한 분리는 Pact 실행에 영향을 미치지는 않지만, 데이터베이스
데이터에서 더 이상 “양자 간 트랜잭션”을 수행할 수 없으므로 분리된 여러
데이터 세트에서의 단일 트랜잭션 수행을 처리하기 위한 새로운 개념이
필요합니다.

.. _confidential-pacts:

Confidential Pacts (기밀 Pact)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pact 기밀성의 중요한 특징은 표적화된 엔터티가 수행할 수 있도록 분리
트랜잭션을 순서대로 조정할 수 있다는 것입니다. 이 내용은 다음 섹션에
설명되어 있습니다.

.. _pacts:

“Pacts”를 이용한 비동기식 트랜잭션 자동화
-----------------------------------------

“Pacts”는 `pact <#defpact>`__\ 라는 단일 바디의 코드로 정의되는
멀티스텝의 순차적 트랜잭션들입니다. 멀티스텝 상호 작용을 Pact 로
정의하면 트랜잭션 참가자들이 합의된 순서로 트랜잭션을 수행하도록 보장할
수 있고, 특별한 “실행 범위”를 제공하여 해당 멀티스텝 상호 작용이
수행되는 동안에만 데이터 리소스를 생성 및 관리하는 데 사용할 수
있습니다.

Pacts는 종료 및 재진입 지정이 여러 프라이빗 *coroutine* 함수의 형태를
가집니다. Pacts는 `스텝들 <#step>`__\ 로 이루어져 해당 블록체인
트랜잭션에서 오직 하나의 스텝만 실행이 되도록 합니다. 스텝들은 엄격한
순서에 따라서만 실행이 가능합니다.

Pacts는 함수 정의와 비슷하게 인수로 정의됩니다. 그러나 인수 값은 초기
스텝을 실행할 때만 평가되며, 이후 인수 값은 후속 스텝에서 변경되지 않은
상태로 계속 사용될 수 있습니다. 후속 스텝들에게 새 값을 공유하기 위해
해당 스텝에서 값을 `yield <pact-functions.html#yield>`__\ 한 뒤 후속
스텝에서 `resume <pact-functions.html#resume>`__ 바인딩 폼을 사용해
복구할 할 수 있습니다.

Pacts는 프라이빗과 퍼블릭 두 가지 맥락 중 하나에서 실행되도록
설계되었습니다. 프라이빗 Pacts는 해당 스텝을 실행하기 위해 단일 엔터티를
식별하는 등 각 스텝마다 표시가 되는 반면에, 퍼블릭 맥락의 스텝은 엔터티
표시자가 없습니다. 하나의 Pact는 퍼블릭 또는 프라이빗 중 오직 하나의
형태만 일률적으로 가질 수 있기 때문에 일부 스텝들에는 엔터티 표시자가
있고 다른 스텝들에는 없는 경우에는 로드 시에 오류가 발생합니다.

퍼블릭 Pacts
~~~~~~~~~~~~

퍼블릭 Pacts는 엄격한 순서에 따라서만 실행이 가능한 스텝들로 이루어져
있습니다. 스텝을 실행할 수 있는 사람의 모든 적용은 스텝 표현식의 코드
내에서 이루어집니다. 모든 스텝은 블록체인에 전송된 CONTINUATION 명령을
통해 트랜잭션의 일부 참가자들이 “수동으로” 시작합니다.

프라이빗 Pacts
~~~~~~~~~~~~~~

프라이빗 Pacts는 순차적으로 실행되는 스텝들로 이루어져 있으며, 여기에서
각 스텝은 해당 스텝에서 제공된 ‘엔터티’ 인수가 선택한 대로 엔터티
노드에서 단 한 번만 실행되고 나머지 엔터티 노드들은 해당 스텝을
“건너뛰기” 합니다. Private Pacts는 초기 스텝이 전송된 이후에 블록체인
플랫폼에서 자동으로 실행되고, 실행 중인 엔터티의 노드는 다음 스텝에 대한
CONTINUATION 명령을 자동으로 전송합니다.

실패, 롤백 및 취소
~~~~~~~~~~~~~~~~~~

실패 처리는 퍼블릭 Pacts와 프라이빗 Pacts에서 크게 다릅니다.

퍼블릭 Pacts의 경우, 한 참가자가 다음 스텝이 실행되기 전에 CANCEL
메시지를 전송하고 있는 동안 이 스텝에서 해당 Pact를 “취소”할 수 있음을
나타내도록 롤백 표현식이 지정되어 있습니다. Pact의 마지막 스텝이
실행되고 나면 해당 Pact 가 완료되면서 롤백이 불가능해집니다. 퍼블릭
스텝에서의 실패는 Pact 가 아닌 트랜잭션의 실패와 다르지 않으며, 모든
변경 사항이 롤백됩니다. 따라서 Pact는 명시적으로 취소가 가능하며, 필요한
모든 취소 옵션을 제공하도록 모델링이 되어야 합니다.

프라이빗 Pacts에서는 스텝의 순차 실행이 블록체인 플랫폼 자체에서
자동으로 이루어집니다. 실패가 발생하면 ROLLBACK 메시지가 실행 중인
엔터티 노드에서 전송되어 이전 스텝에서 지정된 모든 롤백 표현식이 해당
스텝의 엔터티에서 실행되도록 트리거됩니다. 이러한 실패는 새 ROLLBACK
트랜잭션으로서 이전 스텝에 “캐스케이드” 되고, 첫 번째 스텝이 롤백될 때
완료됩니다.

Yield 및 Resume
~~~~~~~~~~~~~~~

`yield <pact-functions.html#yield>`__ 및
`resume <pact-functions.html#resume>`__\ 을 사용하여 한 스텝에서 다음
스텝으로 값을 양도(yield)하고 재개(resume)할 수 있습니다. Public Pact의
경우, 이 값은 블록체인 Pact 범위 내에 유지되기 때문에 위조가
불가능합니다. Private Pact의 경우, 이 값은 실행된 엔터티에서 RESUME
메시지를 통해 전송한 단순한 값입니다.

Pact 실행 범위 및 ``pact-id``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pact 가 시작될 때마다 `pact-id <pact-functions.html#pact-id>`__ 함수를
사용해 검색이 가능한 고유 ID 가 부여됩니다. 이 함수는 현재 실행 중인
Pact의 ID 를 반환하거나 Pact 범위 내에서 실행 중이 아닌 경우에는 실패를
반환합니다. 이러한 메커니즘은 키셋 및 시그니처 사용과 비슷하게 리소스에
대한 액세스를 보호하는 데 사용할 수 있습니다. 이 메커니즘의 전형적인
용도는 해당 Pact의 맥락 내에서만 사용이 가능한 에스크로 계정을
생성함으로써 다양한 사용 사례에서 신뢰할 수 있는 제 3 자가 필요하지
않도록 하는 것입니다.

Pacts 테스트
~~~~~~~~~~~~

`env-entity <pact-functions.html#env-entity>`__,
`env-step <pact-functions.html#env-step>`__ 및
`pact-state <pact-functions.html#pact-state>`__ REPL 함수를 사용해 REPL
스크립트에서 Pact를 테스트하여 Pact 실행을 시뮬레이션할 수 있습니다.
진행 타입의 요청 `continuation Request <#request-yaml>`__ YAML 파일을
``cont`` 페이로드가 포함된 API 요청으로 포맷팅하여 Pact 서버 API 에서
Pact 실행을 시뮬레이션할 수도 있습니다.

.. _dependency-management:

종속 요소 관리 (Dependency Management)
--------------------------------------

Pact는 다른 Pact 모듈에 대한 모듈의 종속 요소를 관리할 수 있도록 다양한
기능을 지원하고 있습니다.

모듈 해시
~~~~~~~~~

로드가 완료된 Pact 모듈은 모듈의 소스 코드 텍스트에서 컴퓨팅된 해시와
연결됩니다. 이러한 모듈 해시는 해당 모듈의 버전을 고유하게 식별합니다.
`describe-module <pact-functions.html#describe-module>`__\ 을 통해 모듈
해시를 검토할 수 있습니다.

::

    pact> (at "hash" (describe-module 'accounts))
    "9d6f4d3acb2fd528206330d09a8926da6abdd9ac5e8c4b24cc35955203f234688c25f9545ead56f783c5269fe4be6a62aa89162caf811142572ac172dc2adb91"

``use``\ 를 통한 모듈 버전 고정
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`use <#use>`__\ 라는 특별 타입을 사용하면 종속 요소 버전을 고정하도록
모듈 해시를 지정할 수 있습니다. 모듈 선언 내에서 사용될 때는 이로 인해
모듈의 해시에 종속 요소 해시 값이 도입됩니다. 따라서 모듈 버전에 대한
업그레이드를 강요하는 “종속 요소 전용” 업그레이드가 가능합니다.

인라이닝된 종속 요소: “Leftpad 방지”
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Leftpad <http://www.koreadaily.com/news/read.asp?art_id=4187383>`__\ 란
“Node.js”에서 발생한 사건으로 가장 하위의 종속 요소가 없어지자 상위
프로그램들이 설치 불가된 상태를 지칭합니다.

Pact에서는 모듈이 로드될 때 모든 사용자 코드 레퍼런스를 인라이닝합니다.
이는 업스트림 정의가 다운스트림 코드에 주입된다는 뜻입니다. 이 시점에서
업스트림 정의는 영구적이며 종속 요소를 업그레이드하는 유일한 방법은 모듈
코드를 다시 로드하는 것입니다.

이러한 영구성은 다운스트림/클라이언트 코드에 효과적이지만, 업스트림
공급자는 로드가 된 후 모듈에서 실행되는 코드를 변경할 수 없습니다.
따라서 업스트림 개발자가 모듈을 개선하거나 새 기능을 도입하기 위해
다운스트림 코드를 업그레이드할 수 없다는 점에서 문제가 됩니다.

해시 블레싱 (Blessing hashes)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위의 문제를 균형있게 해결할 방법이 있습니다. Pact의 업스트림 코드는
런타임 시 그들이 종속된 다운스트림 코드를 비활성화시킬 수 있습니다.
업스트림 개발자들은 모듈 선언시 `블레스(bless) <#bless>`__\ 라는 양식을
이용해 특정 버전의 모듈 테이블 액세스를 제한할 수 있습니다.

.. code:: lisp

    (module provider 'keyset
      (bless "e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94")
      (bless "ca002330e69d3e6b84a46a56a6533fd79d51d97a3bb7cad6c2ff43b354185d6dc1e723fb3db4ae0737e120378424c714bb982d9dc5bbd7a0ab318240ddd18f8d")
      ...
    )

위의 블레스된 해시들은 해당 모듈의 이전 버전 해시들이며 이 해시들이
종속된 업스트림 코드들은 그대로 작동합니다. 하지만 블레스되지 않은
버전이 해당 모듈의 데이터베이스를 액세스한다면 트랜잭션은 실패합니다.

데이터베이스에 액세스하지 않는 “순수한” 코드는 이러한 영향을 받지 않고
기능합니다. 따라서 사소한 유틸리티 함수가 다운스트림 코드의 안전성에
영향을 미치는 “Leftpad” 상황을 방지합니다.

“v2” 모듈을 통한 단계적 업그레이드
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

업스트림 공급자는 블레스 메커니즘을 사용해 새 버전을 표시하도록
업그레이드된 모듈의 이름을 변경하고 기존 모듈을 마지막 버전(그리고
원하는 모든 이전 버전들)만 블레스한 빈 모듈로 새로 교체하는 등 중요한
업그레이드를 단계식으로 수행할 수 있습니다. 새 클라이언트는 “v1” 코드를
가져오는 데 실패하게 되므로 새 버전을 사용해야 합니다. 반면 기존
사용자는 지정된 제한 기간까지 이전 버전을 계속 사용할 수 있습니다. 빈
모듈은 해당 시간대에 자체적으로 업그레이드를 수행할 수 있도록 새 모듈로
사용자 데이터 마이그레이션을 처리하기 위한 마이그레이션 함수를
제공합니다.

문법
====

.. _literals:

리터럴
------

.. _strings:

문자열
~~~~~~

문자열 리터럴은 큰 따옴표를 통해 생성됩니다.

::

    pact> "a string"
    "a string"

또한 문자열은 공백 앞뒤에 백슬래시 기호를 넣어서(양방향은 아님) 다중
라인을 지원합니다.

.. code:: lisp

    (defun id (a)
      "Identity function. \
      \Argument is returned."
      a)

.. _symbols:

기호
~~~~

기호는 함수나 테이블 이름처럼 런타임 시 일부 고유 항목을 표현하는 문자열
리터럴입니다. 기호는 내부적으로 단순한 문자열 리터럴로 표현되므로 그
사용법도 관용적입니다.

기호는 문자 앞의 작은따옴표를 통해 생성되며 공백이나 다중 라인은
지원하지 않습니다.

::

    pact> 'a-symbol
    "a-symbol"

.. _integers:

정수
~~~~

정수 리터럴은 무한대의 자연수입니다. 음수의 경우에는 단항 함수 - 를
사용합니다.

::

    pact> 12345
    12345
    pact> -922337203685477580712387461234
    -922337203685477580712387461234

.. _decimals:

10진수
~~~~~~

10 진 리터럴은 표현 정밀도를 정확하게 나타내는 양의 10진수입니다.

::

    pact> 100.25
    100.25
    pact> -356452.234518728287461023856582382983746
    -356452.234518728287461023856582382983746

.. _booleans:

부울
~~~~

부울은 ``true`` 및 ``false`` 리터럴로 표현됩니다.

::

    pact> (and true false)
    false

.. _lists:

리스트
~~~~~~

리스트 리터럴은 괄호를 사용해 만들 수 있고, 선택에 따라 쉼표로 구분할 수
있습니다. 균일한 리터럴 리스트에는 구문 분석 시 타입이 제공됩니다.

::

    pact> [1 2 3]
    [1 2 3]
    pact> [1,2,3]
    [1 2 3]
    pact> (typeof [1 2 3])
    "[integer]"
    pact> (typeof [1 2 true])
    "list"

.. _objects:

객체
~~~~

객체는 콜론 ``:``\ 를 사용해 키-밸류 쌍을 지정하는 중괄호로 만든
딕셔너리입니다. 특정 애플리케이션(데이터베이스 업데이트)에서는 키가
반드시 문자열이어야 합니다.

::

    pact> { "foo": (+ 1 2), "bar": "baz" }
    (TObject [("foo",3),("bar","baz")])

.. _bindings:

바인딩
~~~~~~

바인딩은 역시 중괄호를 사용해 만든 딕셔너리와 비슷한 양식으로, ``:=``
연산자를 사용해 변수에 데이터베이스 결과를 연결합니다. 이들은
`with-read <pact-functions.html#with-read>`__,
`with-default-read <pact-functions.html#with-default-read>`__,
`bind <pact-functions.html#bind>`__ 및
`resume <pact-functions.html#resume>`__\ 에서 한 행에 명명된 열들과 한
객체의 값들에 변수를 할당하는 데 사용됩니다.

.. code:: lisp

    (defun check-balance (id)
      (with-read accounts id { "balance" := bal }
        (enforce (> bal 0) (format "Account in overdraft: {}" [bal]))))

타입 지정자
-----------

타입 리터럴 또는 사용자 타입 사양 앞에 콜론 ``:`` 연산자를 사용해 타입을
지정할 수 있습니다.

타입 리터럴
~~~~~~~~~~~

-  ``string``
-  ``integer``
-  ``decimal``
-  ``bool``
-  ``time``
-  ``keyset``
-  리스트 타입을 지정하기 위한 ``list`` 또는 ``[type]``
-  스키마를 통해 추가적으로 타입을 지정할 수 있는 ``object``
-  스키마를 통해 추가적으로 타입을 지정할 수 있는 ``table``
-  ``value`` (JSON 값)

스키마 타입 리터럴
~~~~~~~~~~~~~~~~~~

`defschema <#defschema>`__\ 를 통해 정의된 스키마는 이름을 중괄호로 묶어
참조할 수 있습니다.

.. code:: lisp

    table:{accounts}
    object:{person}

타입을 가질 수 있는 형태
~~~~~~~~~~~~~~~~~~~~~~~~

함수 인수 및 반환 타입
^^^^^^^^^^^^^^^^^^^^^^

.. code:: lisp

    (defun prefix:string (pfx:string str:string) (+ pfx str))

let 변수
^^^^^^^^

.. code:: lisp

    (let ((a:integer 1) (b:integer 2)) (+ a b))

테이블 및 객체
^^^^^^^^^^^^^^

테이블 및 객체는 스키마 타입 리터럴만 가져올 수 있습니다.

.. code:: lisp

    (deftable accounts:{account})

    (defun get-order:{order} (id) (read orders id))

상수 (Consts)
^^^^^^^^^^^^^

.. code:: lisp

    (defconst PENNY:decimal 0.1)

.. _special-forms:

특별한 양식
-----------

문서 및 메타데이터
~~~~~~~~~~~~~~~~~~

`defun <#defun>`__ 같은 다수의 특별 양식은 다큐멘테이션 용도의 문자열을
수락합니다.

.. code:: lisp

    (defun average (a b)
      "take the average of a and b"
      (/ (+ a b) 2))

또 다른 방법으로는 ``@`` 접두사를 통해 메타데이터를 지정할 수 있습니다.
지원하는 메타데이터에는 ``@doc`` 또는 ``@model``\ 이 있으며 Pact
도구에서 구현이 올바른지 검증하기 위해서만 사용됩니다. update

.. code:: lisp

    (defun average (a b)
      @doc   "take the average of a and b"
      @model (property (= (+ a b) (* 2 result)))
      (/ (+ a b) 2))

사실 ``"foo"``\ 라는 다큐멘테이션용 문자열은 ``@doc "foo"``\ 을 줄인것에
불과합니다.

*속성* 에 대한 자세한 정보는 `Pact 속성 검사
시스템 <pact-properties.html>`__\ 을 확인하세요.

bless
~~~~~

::

    (bless HASH)

모듈 선언 내에서 HASH 로 식별되는 해당 모듈의 이전 버전을 블레스합니다.
블레스 메커니즘에 대한 내용은 `종속 요소
관리 <#dependency-management>`__\ 를 참조하세요.

.. code:: lisp

    (module provider 'keyset
      (bless "e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94")
      (bless "ca002330e69d3e6b84a46a56a6533fd79d51d97a3bb7cad6c2ff43b354185d6dc1e723fb3db4ae0737e120378424c714bb982d9dc5bbd7a0ab318240ddd18f8d")
      ...
    )

defun
~~~~~

.. code:: lisp

    (defun NAME ARGLIST [DOC-OR-META] BODY...)

DOC-OR-META 옵션을 통해 ARGLIST 인수를 수락하도록 NAME 을 함수로
정의합니다. 인수는 BODY 의 범위 내, 즉 하나 이상의 표현식에 있습니다.

.. code:: lisp

    (defun add3 (a b c) (+ a (+ b c)))

    (defun scale3 (a b c s)
      "multiply sum of A B C times s"
      (* s (add3 a b c)))

defconst
~~~~~~~~

.. code:: lisp

    (defconst NAME VALUE [DOC-OR-META])

DOC-OR-META 옵션을 통해 NAME 을 VALUE 로 정의합니다. 모듈 로드 시 값이
평가되고 “메모이제이션”됩니다.

.. code:: lisp

    (defconst COLOR_RED="#FF0000" "Red in hex")
    (defconst COLOR_GRN="#00FF00" "Green in hex")
    (defconst PI 3.14159265 "Pi to 8 decimals")

defpact
~~~~~~~

::

    (defpact NAME ARGLIST [DOC-OR-META] STEPS...)

별개의 트랜잭션을 위해 고안된 멀티스텝 계산인 *pact* 로 NAME 을
정의합니다. `defun <#defun>`__\ 와 동일하지만 바디는 엄격한 순서에 따라
실행되는 `스텝들 <#step>`__\ 로 이루어져야 합니다. 스텝은
“Public”(엔터티 표시자 없음) 또는 “Private”(엔터티 표시자 있음)
타입이어야 합니다. Private 스텝에서는 실패로 인해 역순서 “롤백
캐스케이드”가 발생합니다.

.. code:: lisp

    (defpact payment (payer payer-entity payee
                      payee-entity amount)
      (step-with-rollback payer-entity
        (debit payer amount)
        (credit payer amount))
      (step payee-entity
        (credit payee amount)))

defschema
~~~~~~~~~

::

    (defschema NAME [DOC-OR-META] FIELDS...)

NAME 을 FIELDS 로 이루어진 *스키마* 로 정의합니다. 각 필드는
``FIELDNAME[:FIELDTYPE]``\ 의 타입을 가지고 있습니다.

.. code:: lisp

    (defschema accounts
      "Schema for accounts table".
      balance:decimal
      amount:decimal
      ccy:string
      data)

deftable
~~~~~~~~

::

    (deftable NAME[:SCHEMA] [DOC-OR-META])

NAME 을 데이터베이스 함수에 사용되는 *테이블* 으로 정의합니다. 테이블은
여전히 `create-table <pact-functions.html#create-table>`__\ 으로
생성해야 합니다.

let
~~~

::

    (let (BINDPAIR [BINDPAIR [...]]) BODY)

BINDPAIR 의 변수들이 BODY 안의 범위에 있도록 바인딩합니다. BINDPAIR 내의
변수들은 같은 let 바인딩(이 경우에는 `let\* <#letstar>`__)에서 이전에
선언한 변수를 참조할 수 없습니다.

.. code:: lisp

    (let ((x 2)
          (y 5))
      (* x y))
    > 10

.. _letstar:

let\*
~~~~~

::

    (let* (BINDPAIR [BINDPAIR [...]]) BODY)

BINDPAIR 의 변수들이 BODY 의 범위에 있도록 바인딩합니다. 변수는 같은 let
에서 이전에 선언한 BINDPAIR 를 참조할 수 있습니다. ``let*``\ 은 컴파일
시 각 BINDPAIR 에 대한 중첩된 ``let`` 호출로까지 확장됩니다. 따라서
가능하면 ``let``\ 을 사용하는 것이 좋습니다.

.. code:: lisp

    (let* ((x 2)
           (y (* x 10)))
      (+ x y))
    > 22

step
~~~~

::

    (step EXPR)
    (step ENTITY EXPR)

이전 스텝들은 이전 트랜잭션에서, 이후 스텝들은 이후 트랜잭션에서 실행이
되도록 `defpact <#defpact>`__ 내에서 스텝을 정의합니다. ENTITY 인수의
표시는 해당 스텝이 기밀 트랜잭션용이라는 것을 의미합니다. 오직 ENTITY 만
해당 스텝을 실행하며 다른 참가자들은 defpact를 포함해 지정된 실행 순서에
따라 해당 스텝을 “건너뛰기” 합니다.

step-with-rollback
~~~~~~~~~~~~~~~~~~

::

    (step-with-rollback EXPR ROLLBACK-EXPR)
    (step-with-rollback ENTITY EXPR ROLLBACK-EXPR)

`defpact <#defpact>`__ 내에서 `스텝 <#step>`__\ 을 정의합니다. 이는
스텝과 비슷하지만 ROLLBACK-EXPR 을 지정합니다. ENTITY 가 있으면 후속
스텝이 실패할 때 실패한 스텝에서 첫 번째 스텝으로로 돌아가는 역순서
“롤백 캐스케이드”의 일환으로 ROLLBACK-EXPR 만 실행됩니다. ENTITY 가
없으면 ROLLBACK-EXPR 함수가 참가자에 의해 명시적으로 실행되는 “취소
함수” 역할을 합니다.

use
~~~

::

    (use MODULE)
    (use MODULE HASH)

기존 MODULE 을 네임스페이스로 가져옵니다. 최상위 수준에서, 또는 모듈
선언 내에서만 사용이 가능합니다. MODULE 은 문자열, 기호 또는 원자(드문
경우)가 될 수 있습니다. HASH 를 통해 모듈 해시가 HASH 와 일치하는지
확인합니다(일치하지 않는 경우에는 실패).
`describe-module <pact-functions.html#describe-module>`__\ 을 사용해
체인에서 로드된 모듈의 해시를 쿼리합니다.

.. code:: lisp

    (use accounts)
    (transfer "123" "456" 5 (time "2016-07-22T11:26:35Z"))
    "Write succeeded"

module
~~~~~~

::

    (module NAME KEYSET [DOC-OR-META] DEFS...)

DOC-OR-META 옵션을 통해 모듈 NAME(키셋 KEYSET 에 의해 보호)을 정의 및
설치합니다. DEFS 는 반드시 `defun <#defun>`__ 또는
`defpact <#defpact>`__ 표현식이어야 합니다

.. code:: lisp

    (module accounts 'accounts-admin
      "Module for interacting with accounts"

      (defun create-account (id bal)
       "Create account ID with initial balance BAL"
       (insert accounts id { "balance": bal }))

      (defun transfer (from to amount)
       "Transfer AMOUNT from FROM to TO"
       (with-read accounts from { "balance": fbal }
        (enforce (<= amount fbal) "Insufficient funds")
         (with-read accounts to { "balance": tbal }
          (update accounts from { "balance": (- fbal amount) })
          (update accounts to { "balance": (+ tbal amount) }))))
    )

.. _expression:

표현식
------

표현식은 `리터럴 <#literals>`__, 원자, s-표현식 또는 레퍼런스일 수
있습니다.

.. _atom:

원자(atoms)
~~~~~~~~~~~

원자는 문자 또는 허용되는 기호로 시작되고 문자, 숫자 및 허용 기호가
포함되어 있으며 예약어가 아닌 bareword 입니다. 허용 기호는
``%#+-_&$@<>=?*!|/``\ 입니다. 원자는 `defun <#defun>`__,
`defpact <#defpact>`__, `binding <#bindings>`__ 양식에 의해 바인딩되는
변수나 `use <#use>`__\ 를 통해 네임스페이스로 가져온 기호로 귀결되어야
합니다.

.. _sexp:

S-표현식
~~~~~~~~

S-표현식은 괄호로 이루어져 있는데, 첫 번째 원자는 표현식이 `특별
양식 <#special-forms>`__\ 인지, 아니면 첫 번째 원자가 정의를 참조해야
하는 함수 애플리케이션인지 결정합니다.

.. _partialapplication:

부분 애플리케이션
^^^^^^^^^^^^^^^^^

필요한 수보다 인수가 적은 애플리케이션은 어떤 맥락에서 함수에서 유효한
*부분 애플리케이션* 입니다. 그러나 이러한 애플리케이션은 Pact의 `함수형
스타일 함수(functional-style
functions) <#functional-concepts>`__\ 에서만 지원되고, 그 외의 경우는
런타임 오류가 발생하게 됩니다.

.. _references:

레퍼런스
~~~~~~~~

레퍼런스는 모듈 정의를 직접 결정하기 위해 점 ``.`` 으로 결합된 두 개의
원자입니다.References are multiple atoms joined by a dot.

::

    pact> accounts.transfer
    "(defun accounts.transfer (src,dest,amount,date) \"transfer AMOUNT from
    SRC to DEST\")"
    pact> transfer
    Eval failure:
    transfer<EOF>: Cannot resolve transfer
    pact> (use 'accounts)
    "Using \"accounts\""
    pact> transfer
    "(defun accounts.transfer (src,dest,amount,date) \"transfer AMOUNT from
    SRC to DEST\")"

레퍼런스가 더 빨리 귀결을 한다는 점에서 트랜잭션에서는 ``use`` 보다
레퍼런스가 선호됩니다. 하지만 모듈 정의에서는 가독성이 좋은 ``use``\ 가
선호됩니다.

시간 형식
=========

Pact는 신속한 시간 값 계산을 위해 Haskell `thyme
라이브러리 <http://hackage.haskell.org/package/thyme>`__\ 를 활용합니다.
`parse-time <pact-functions.html#parse-time>`__ 및
`format-time <pact-functions.html#format-time>`__ 함수는 다음과 같이
몇몇 확장자와 함께 GNU ``strftime``\ 에서 파생된 타입 코드를 수락합니다.

``%%`` - 리터럴 ``"%"``

``%z`` - RFC 822/ISO 8601:1988 스타일 숫자 시간대(예: ``"-0600"`` 또는
``"+0100"``)

``%N`` - ISO 8601 스타일 숫자 시간대(예: ``"-06:00"`` 또는 ``"+01:00"``)
/EXTENSION/

``%Z`` - 시간대 이름

``%c`` - 현재 로캘에서 선호되는 캘린더 시간 표현. ‘dateTimeFmt’
``locale`` 형태(예: ``%a %b %e %H:%M:%S %Z %Y``)

``%R`` - ``%H:%M``\ 와 동일

``%T`` - ``%H:%M:%S``\ 와 동일

``%X`` - 현재 로캘에서 선호되는 하루의 시간 표현. ‘timeFmt’ ``locale``
형태(예: ``%H:%M:%S``)

``%r`` - 현재 로캘의 AM/PM 형식을 사용하는 완전한 캘린더 시간.
‘time12Fmt’ ``locale`` 형태(예: ``%I:%M:%S %p``)

``%P`` - (‘amPm’ ``locale``)에서의 반나절, 소문자로 변환, ``"am"``,
``"pm"``

``%p`` - (‘amPm’ ``locale``)에서의 반나절, 소문자로 변환, ``"am"``,
``"pm"``

``%H`` - 하루의 시간(24 시간), 두 개 문자가 0 으로 채워짐,
``"00"``–``"23"``

``%k`` - 하루의 시간(24 시간), 두 개 문자가 공백으로 채워짐,
``" 0"``–``"23"``

``%I`` - 하루의 시간(12 시간), 두 개 문자가 0 으로 채워짐,
``"01"``–``"12"``

``%l`` - 하루의 시간(12 시간), 두 개 문자가 공백으로 채워짐,
``" 1"``–``"12"``

``%M`` - 시간의 분, 두 개 문자가 0 으로 채워짐, ``"00"``–``"59"``

``%S`` - 분의 초(10 진 부분이 없음), 두 개 문자가 0 으로 채워짐,
``"00"``–``"60"``

``%v`` - 초의 마이크로세컨드, 6 개 문자가 0 으로 채워짐,
``"000000"``–``"999999"``. /EXTENSION/

``%Q`` - 초의 10 진 소수점 및 가수 부분, 최대 6 개의 초 10 진 숫자, 후행
제로(trailing zero) 없음. 전체 초 수에 대해 ``%Q``\ 는 빈 문자열을
만듭니다. /EXTENSION/

``%s`` - Unix epoch 이후의 전체 초 수. Unix epoch 이전의 시간에서는 이
수가 음수입니다. ``%s.%q`` 및 ``%s%Q``\ 에서 10진수는 음수가 아니라
양수입니다. 예를 들어 Unix epoch 이전의 0.9 초는 ``%s%Q``\ 에서
``"-1.1"``\ 로 포맷팅됩니다.

``%D`` - ``%m\/%d\/%y``\ 와 동일

``%F`` - ``%Y-%m-%d``\ 와 동일

``%x`` - ‘dateFmt’ 형태 ``locale``\ (예: ``%m\/%d\/%y``)

``%Y`` - 연도, 숫자 패딩 없음.

``%y`` - 세기의 연도, 두 개 문자가 0 으로 채워짐, ``"00"``–``"99"``

``%C`` - 세기, 숫자 패딩 없음.

``%B`` - 월 이름, 긴 형태(‘months’ ``locale``\ 의 ‘fst’),
``"January"``–``"December"``

``%b``, ``%h`` - 월 이름, 짧은 형태(‘months’ ``locale``\ 의 ‘snd’),
``"Jan"``–``"Dec"``

``%m`` - 월, 두 개 문자가 0 으로 채워짐, ``"01"``–``"12"``

``%d`` - 일, 두 개 문자가 0 으로 채워짐, ``"01"``–``"31"``

``%e`` - 월의 일, 두 개 문자가 공백으로 채워짐, ``"01"``–``"31"``

``%j`` - 연의 일, 세 개 문자가 0 으로 채워짐, ``"001"``–``"366"``

``%G`` - 주 날짜 타입을 위한 연도, 숫자 패딩 없음.

``%g`` - 주 날짜 타입을 위한 세기의 연도, 두 개 문자가 0 으로 채워짐,
``"00"``–``"99"``

``%f`` - 주 날짜 타입을 위한 세기, 숫자 패딩 없음. /EXTENSION/

``%V`` - 주 날짜 타입을 위한 연도의 주, 두 개 문자가 0 으로 채워짐,
``"01"``–``"53"``

``%u`` - 주 날짜 타입을 위한 주의 날짜, ``"1"``–``"7"``

``%a`` - 주의 날짜, 짧은 형태(‘wDays’, ``locale``\ 의 ‘snd’),
``"Sun"``–``"Sat"``

``%A`` - 주의 일, 긴 형태(‘wDays’ ``locale``\ 의 ‘fst’),
``"Sunday"``–``"Saturday"``

``%U`` - 일요일에 주가 시작되는 연도의 주(예: ‘sundayStartWeek’), 두 개
문자가 0 으로 채워짐, ``"00"``–``"53"``

``%w`` - 주의 일 수, ``"0"`` (= 일요일) – ``"6"`` (= 토요일)

``%W`` - 월요일에 주가 시작되는 연도의 주(예:
‘Data.Thyme.Calendar.WeekdayOfMonth.mondayStartWeek’), 두 개 문자가 0
으로 채워짐, ``"00"``–``"53"``

참고: ``%q`` (피코세컨드, 0 으로 채워짐)은 제대로 작동하지 않기 때문에
여기에 문서화되지 않았습니다.

기본 형식 및 JSON 직렬화
------------------------

기본 형식은 UTC ISO8601 날짜+시간 타입인 “%Y-%m-%dT%H:%M:%SZ”으로,
`time <pact-functions.html#id4>`__ 함수에서 허용하는 타입입니다. 시간
객체는 내부적으로 최대 마이크로세컨드의 분해능을 지원하지만 Pact
해석기에서 JSON 으로 반환된 값이 기본 타입으로 직렬화됩니다. 더 높은
분해능을 원할 때는 ``%v`` 및 관련 함수로 시간을 명시적으로 포맷팅합니다.

예시
----

ISO8601
~~~~~~~

::

    pact> (format-time "%Y-%m-%dT%H:%M:%S%N" (time "2016-07-23T13:30:45Z"))
    "2016-07-23T13:30:45+00:00"

RFC822
~~~~~~

::

    pact> (format-time "%a, %_d %b %Y %H:%M:%S %Z" (time "2016-07-23T13:30:45Z"))
    "Sat, 23 Jul 2016 13:30:45 UTC"

YYYY-MM-DD hh:mm:ss.000000
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    pact> (format-time "%Y-%m-%d %H:%M:%S.%v" (add-time (time "2016-07-23T13:30:45Z") 0.001002))
    "2016-07-23 13:30:45.001002"

데이터베이스 직렬화 포맷 (Database Serialization Format)
========================================================

베타 경고 IMPORTANT EXPERIMENTAL / BETA WARNING
-----------------------------------------------

이 섹션에서는 Pact 2.4.\* 버전부터 시작해서 데이터베이스 직렬화 타입을
문서화합니다. 그러나 이 타입은 여전히 BETA 상태에 있으며, 현재는 이
데이터를 직접 내보내는 구체적인 RDBMS 백엔드 및 응용 프로그램에서 사용을
시작한 상태입니다.

그 결과, 우리는 이전 버전과의 호환성에 대해 약속할 수 없으며 향후 버전의
개선된 타입으로 마이그레이션할 수 있는 권리를 보유하고 있습니다. Pact의
API 안정성에 있어 가장 우선순위를 두는 부분은 클라이언트가 경험하는
호환성 및 성능으로, 백엔드 가져오기는 여전히 실험적인 기능입니다.

이러한 타입들은 미래에 이전 버전과의 호환성이 보장된다 하더라도
안정화되는 것을 기대하지 않습니다.

JSON 값이 포함된 키-밸류 타입
-----------------------------

Pact는 JSON 으로 표현된 모든 값에서 2 열 키-밸류 구조로 된 백엔드
데이터베이스에 모든 값을 저장합니다. 이는 투명성과 이식성을 높이고자
등장한 접근 방식입니다.

*투명성*: JSON 은 사람이 읽을 수 있는 타입이기 때문에 값을 시각적으로
확인할 수 있습니다.

*이식성*: JSON 은 작성 시 거의 모든 데이터베이스 백엔드에서 강력한 지원
기능을 활용합니다(2018). 키-밸류 구조 덕분에 RocksDB 같은 비 RDBMS
백엔드도 사용할 수 있고, 간단한 기본 키 구조에서 SQL DDL 을 매우
간단하게 유지할 수 있습니다. 인덱싱은 지원 또는 요구되지 않습니다.

Pact 데이터타입 코덱
--------------------

지원되는 모든 Pact 데이터 타입에서 이들은 직렬화 속도와 정확도를
높이도록 설계된 프론트 엔드 API 에서 사용되는 JSON 타입과 다른 특수
코드를 사용하여 JSON 에 인코딩됩니다.

정수
~~~~

크지 않은 정수의 경우에는 값이 JSON 숫자로 직접 인코딩됩니다.

JSON/Javascript 에서 무엇을 “큰 정수”로 간주할 것이냐에 대해서는 논쟁의
여지가 있기 때문에 우리는
`이곳 <http://blog.vjeux.com/2010/javascript/javascript-max_int-number-limits.html>`__\ 에
지정된 것 과 같은 범위 ``[-2^53 .. 2^53]``\ 를 사용하고 있습니다. 큰
정수의 경우, 문자열화된 정수 값으로 JSON 단집합 객체를 인코딩하고
있습니다.

.. code:: javascript

    /* small integers are just a number */
    1
    /* large integers are objects */
    { "_P_int": "123..." /* integer string representation */
    }

10진수
~~~~~~

10진수는 *places* 및 *mantissa* 를 사용해 `Haskell Decimal
형식 <https://hackage.haskell.org/package/Decimal-0.5.1/docs/Data-Decimal.html#t:DecimalRaw>`__\ 에
따라 인코딩됩니다.

.. code:: javascript

    { "_P_decp": 4     /* decimal places */
    , "_P_decm": 15246 /* decimal mantissa, encoded using INTEGER format */
    }

mantissa 값은 위에서 설명한 정수 타입을 사용합니다. 10진수 문서에
설명되어 있듯이 이 값은 다음과 같이 계산할 수 있습니다.

::

    MANTISSA / (10 ^ PLACES)

부울
~~~~

부울은 JSON 부울으로 저장됩니다.

문자열
~~~~~~

문자열은 JSON 문자열로 저장됩니다.

시간
~~~~

시간은 Modified Julian Day 값과 마이크로세컨드 값을 포착하여 JSON 객체에
저장됩니다.

.. code:: javascript

    { "_P_timed": 234 /* "modified julian day value */
    , "_P_timems": 32495874 /* microseconds, encoded using INTEGER format */
    }

MJD 변환을 위한 제안 사항은
`여기 <https://stackoverflow.com/questions/11889553/convert-modified-julian-date-to-utc>`__\ 에서
확인할 수 있습니다.

JSON 값/Blob
~~~~~~~~~~~~

미처리 JSON blob 는 수정되지 않은 상태로 컨테이너 객체에 인코딩됩니다.

.. code:: javascript

    { "_P_val": { "foo": "bar" } /* unmodified user JSON object */
    }

키셋
~~~~

키셋은 키 리스트와 predicate 이름을 JSON 객체에 저장합니다.

.. code:: javascript

    { "_P_keys": ["key1","key2"] /* public key string representations */
    , "_P_pred": "keys-all"      /* predicate function name */
    }

모듈(사용자) 테이블
-------------------

Pact 코드에 지정된 각 모듈 테이블에서 “데이터 테이블”과 “트랜잭션
테이블,” 두 개의 백엔드 테이블이 생성됩니다.

열 이름
~~~~~~~

모든 키 값 테이블의 이름은 단순하게 **t_key** 및 **t_value**\ 입니다.

사용자 데이터 테이블
~~~~~~~~~~~~~~~~~~~~

데이터 테이블은 현재 테이블 상태에 대해 CRUD 스타일의 액세스를
지원합니다.

-  **명명**: ``USER_[module]_[table]``.
-  **키 형식**: 키는 텍스트/VARCHAR 이고, 지원되는 최대 길이는 백엔드에
   따라 다릅니다.
-  **값 형식**: 사용자 지정 키와 코덱 변환 값을 가진 JSON 객체입니다.

사용자 트랜잭션 테이블
~~~~~~~~~~~~~~~~~~~~~~

트랜잭션 테이블에는 테이블에 대한 모든 업데이트가 기록됩니다.

-  **명명**: ``TX_[module]_[table]``
-  **키 형식**: 키는 백엔드 고유의 BIGINT 값을 사용하는 정수이며 기록
   중인 트랜잭션 ID 를 반영합니다.
-  **값 형식**: 특정 트랜잭션의 업데이트로 이루어진 JSON array 입니다.

업데이트 형식은 JSON 객체입니다.

.. code:: javascript

    { "table": "name"  /* user-visible table name (not backend table name) */
    , "key": "123"     /* update string key */
    , "value": { ... } /* The new JSON row value. Entire row is captured. */

JSON 행 값은 사용자 데이터 테이블에서 확인한 것과 같은 인코딩을
사용합니다.

.. _builtins:

내장 함수
=========

.. _General:

일반
----

at
~~

*idx* ``integer`` *list* ``[<l>]`` *→* ``<a>``

*idx* ``string`` *object* ``object:<{o}>`` *→* ``<a>``

IDX 에서 LIST 를 인덱싱하거나 OBJECT 에서 키 IDX 를 통해 값을 얻습니다

.. code:: lisp

    pact> (at 1 [1 2 3])
    2
    pact> (at "bar" { "foo": 1, "bar": 2 })
    2

bind
~~~~

*src* ``object:<{row}>`` *binding* ``binding:<{row}>`` *→* ``<a>``

특수 양식이 후속 바디 문에 BINDINGS 를 통해 바인딩 된 객체로 SRC 를
평가합니다.

.. code:: lisp

    pact> (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
    1

compose
~~~~~~~

*x* ``(x:<a> -> <b>)`` *y* ``(x:<b> -> <c>)`` *value* ``<a>``
*→* ``<c>``

X 는 VALUE 에서, Y 는 X 의 결과에서 작동하도록 X 와 Y 를 작성합니다.

.. code:: lisp

    pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
    ["dog" "has" "fleas"]

constantly
~~~~~~~~~~

*value* ``<a>`` *ignore1* ``<b>`` *→* ``<a>``

*value* ``<a>`` *ignore1* ``<b>`` *ignore2* ``<c>`` *→* ``<a>``

*value* ``<a>`` *ignore1* ``<b>`` *ignore2* ``<c>`` *ignore3* ``<d>``
*→* ``<a>``

나태하게(Lazily) 인수 IGNORE\* 을 무시하고 VALUE 를 반환합니다.

.. code:: lisp

    pact> (filter (constantly true) [1 2 3])
    [1 2 3]

contains
~~~~~~~~

*value* ``<a>`` *list* ``[<a>]`` *→* ``bool``

*key* ``<a>`` *object* ``object:<{o}>`` *→* ``bool``

*value* ``string`` *string* ``string`` *→* ``bool``

LIST 또는 STRING 에 VALUE 가 포함되어 있는지, 또는 OBJECT 에 KEY 항목이
있는지 테스트합니다.

.. code:: lisp

    pact> (contains 2 [1 2 3])
    true
    pact> (contains 'name { 'name: "Ted", 'age: 72 })
    true
    pact> (contains "foo" "foobar")
    true

drop
~~~~

*count* ``integer`` *list* ``<a[[<l>],string]>``
*→* ``<a[[<l>],string]>``

*keys* ``[string]`` *object* ``object:<{o}>`` *→* ``object:<{o}>``

LIST(또는 문자열)에서 COUNT 값을, OBJECT 에서 KEYS 에 키를 가지고 있는
항목들을 삭제합니다. COUNT 가 음수인 경우 끝에서부터 삭제합니다.

.. code:: lisp

    pact> (drop 2 "vwxyz")
    "xyz"
    pact> (drop (- 2) [1 2 3 4 5])
    [1 2 3]
    pact> (drop ['name] { 'name: "Vlad", 'active: false})
    {"active": false}

enforce
~~~~~~~

*test* ``bool`` *msg* ``string`` *→* ``bool``

순수 함수 TEST 가 false 를 반환할 경우 MSG 와 함께 트랜잭션을
실패시킵니다. 그 외의 경우에는 true 를 반환합니다.

.. code:: lisp

    pact> (enforce (!= (+ 2 2) 4) "Chaos reigns")
    <interactive>:0:0: Chaos reigns

enforce-one
~~~~~~~~~~~

*msg* ``string`` *tests* ``[bool]`` *→* ``bool``

순서대로 TESTS 를 실행합니다(순수한 맥락, 키셋 적용). 모두 실패하면
트랜잭션이 실패합니다. 첫 번째 성공 시 쇼트 서킷이 이루어집니다.

.. code:: lisp

    pact> (enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])
    true

enforce-pact-version
~~~~~~~~~~~~~~~~~~~~

*min-version* ``string`` *→* ``bool``

*min-version* ``string`` *max-version* ``string`` *→* ``bool``

MIN-VERSION 이상 또는 MAX-VERSION 이하로 런타임 Pact 버전을 적용합니다.
버전 값은 왼쪽부터 숫자가 매칭됩니다(예를 들어 ‘2’, ‘2.2’ 및 ‘2.2.3’에서
모두 ‘2.2.3’이 지원).

.. code:: lisp

    pact> (enforce-pact-version "2.3")
    true

최상위 수준에서만 사용 가능: 모듈 코드에 적용 시 실패합니다.

filter
~~~~~~

*app* ``(x:<a> -> bool)`` *list* ``[<a>]`` *→* ``[<a>]``

각 요소에 APP 를 적용하여 LIST 를 필터링합니다. True 가 반환되는 요소는
보관됩니다.

.. code:: lisp

    pact> (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
    ["dog" "has" "fleas"]

fold
~~~~

*app* ``(x:<a> y:<b> -> <a>)`` *init* ``<a>`` *list* ``[<b>]``
*→* ``<a>``

INIT 를 시작으로 마지막 결과 와 요소에 APP 을 적용하여 LIST 를
반복적으로 reduce 합니다.

.. code:: lisp

    pact> (fold (+) 0 [100 10 5])
    115

format
~~~~~~

*template* ``string`` *vars* ``list`` *→* ``string``

{}를 사용해 TEMPLATE 에 VARS 를 삽입합니다.

.. code:: lisp

    pact> (format "My {} has {}" ["dog" "fleas"])
    "My dog has fleas"

hash
~~~~

*value* ``<a>`` *→* ``string``

VALUE 의 BLAKE2b 512-비트 해시를 계산합니다. 문자열은 직접 변환이
되지만, 다른 값들은 JSON 표현식을 사용해 변환됩니다.

.. code:: lisp

    pact> (hash "hello")
    "e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"
    pact> (hash { 'foo: 1 })
    "61d3c8775e151b4582ca7f9a885a9b2195d5aa6acc58ddca61a504e9986bb8c06eeb37af722ad848f9009053b6379677bf111e25a680ab41a209c4d56ff1e183"

identity
~~~~~~~~

*value* ``<a>`` *→* ``<a>``

제공된 값을 반환합니다.

.. code:: lisp

    pact> (map (identity) [1 2 3])
    [1 2 3]

if
~~

*cond* ``bool`` *then* ``<a>`` *else* ``<a>`` *→* ``<a>``

COND 를 테스트해서 true 이면 THEN 을 평가하고 그렇지 않으면 ELSE 를
평가합니다.

.. code:: lisp

    pact> (if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
    "Sanity prevails"

length
~~~~~~

*x* ``<a[[<l>],string,object:<{o}>]>`` *→* ``integer``

X(리스트, 문자열 또는 객체 타입)의 길이를 계산합니다.

.. code:: lisp

    pact> (length [1 2 3])
    3
    pact> (length "abcdefgh")
    8
    pact> (length { "a": 1, "b": 2 })
    2

list
~~~~

*elems* ``*`` *→* ``list``

ELEMS 에서 리스트를 생성합니다. Pact 2.1.1 에서는 사용이 중단되었고
대신에 리터럴 리스트가 지원됩니다.

.. code:: lisp

    pact> (list 1 2 3)
    [1 2 3]

list-modules
~~~~~~~~~~~~

*→* ``[string]``

로딩에 사용할 수 있는 모듈을 나열합니다.

최상위 수준에서만 사용 가능: 모듈 코드에 적용 시 실패합니다.

make-list
~~~~~~~~~

*length* ``integer`` *value* ``<a>`` *→* ``[<a>]``

VALUE 를 LENGTH 만큼 반복하여 리스트를 생성합니다.

.. code:: lisp

    pact> (make-list 5 true)
    [true true true true true]

map
~~~

*app* ``(x:<b> -> <a>)`` *list* ``[<b>]`` *→* ``[<a>]``

LIST 의 각 요소에 APP 을 적용하여 결과 리스트를 반환합니다.

.. code:: lisp

    pact> (map (+ 1) [1 2 3])
    [2 3 4]

pact-id
~~~~~~~

*→* ``integer``

현재 Pact 실행 중에 호출된 경우에는 ID 가 반환되고, 그렇지 않은 경우에는
실패합니다.

pact-version
~~~~~~~~~~~~

*→* ``string``

현재 Pact의 빌드 버전을 획득합니다.

.. code:: lisp

    pact> (pact-version)
    "2.6.0"

최상위 수준에서만 사용 가능: 모듈 코드에 적용 시 실패합니다.

read-decimal
~~~~~~~~~~~~

*key* ``string`` *→* ``decimal``

메시지 데이터 바디의 상위 수준부터 KEY 문자열 또는 숫자 값을 10진수
형태로 파싱합니다.

.. code:: lisp

    (defun exec ()
       (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))

read-integer
~~~~~~~~~~~~

*key* ``string`` *→* ``integer``

메시지 데이터 바디의 상위 수준부터 KEY 문자열 또는 숫자 값을 정수 형태로
파싱합니다.

.. code:: lisp

    (read-integer "age")

read-msg
~~~~~~~~

*→* ``<a>``

*key* ``string`` *→* ``<a>``

메시지 데이터 바디 또는 데이터 바디 자체(메시지가 제공되지 않는 경우)의
상위 수준부터 KEY 를 읽어들입니다. Pact 타입에 대한 값을 강제
변환합니다(String -> 문자열, Number -> 정수, Boolean -> 부울, List ->
리스트, Object -> 객체). 상위 수준의 값들은 JSON 타입인 ’value’로
제공됩니다.

.. code:: lisp

    (defun exec ()
       (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))

remove
~~~~~~

*key* ``string`` *object* ``object:<{o}>`` *→* ``object:<{o}>``

OBJECT 에서 KEY 에 대한 항목을 제거합니다.

.. code:: lisp

    pact> (remove "bar" { "foo": 1, "bar": 2 })
    {"foo": 1}

resume
~~~~~~

*binding* ``binding:<{y}>`` *body* ``*`` *→* ``<a>``

특수 형식이 pact의 이전 스텝 실행에서 yield 된 객체 값에 바인딩됩니다.

reverse
~~~~~~~

*list* ``[<a>]`` *→* ``[<a>]``

리스트를 반대로 뒤집습니다.

.. code:: lisp

    pact> (reverse [1 2 3])
    [3 2 1]

sort
~~~~

*values* ``[<a>]`` *→* ``[<a>]``

*fields* ``[string]`` *values* ``[object:<{o}>]`` *→* ``[object:<{o}>]``

모노타입 리스트를 원시 VALUES 로 정렬하거나 제공되는 FIELDS 리스트를
통해 객체를 정렬합니다.

.. code:: lisp

    pact> (sort [3 1 2])
    [1 2 3]
    pact> (sort ['age] [{'name: "Lin",'age: 30} {'name: "Val",'age: 25}])
    [{"name": "Val", "age": 25} {"name": "Lin", "age": 30}]

str-to-int
~~~~~~~~~~

*str-val* ``string`` *→* ``integer``

*base* ``integer`` *str-val* ``string`` *→* ``integer``

STR_VAL 의 정수 값을 명시되지 않았다면 10진수, 명시되었다면 BASE의
진법으로 계산합니다. STR-VAL 는 <= 128 chars의 길이여야하며 BASE 는 2
에서 16 사이여야 합니다. 각 숫자는 진법의 올바른 범위에 있어야 합니다.

.. code:: lisp

    pact> (str-to-int 16 "abcdef123456")
    188900967593046
    pact> (str-to-int "123456")
    123456

take
~~~~

*count* ``integer`` *list* ``<a[[<l>],string]>``
*→* ``<a[[<l>],string]>``

*keys* ``[string]`` *object* ``object:<{o}>`` *→* ``object:<{o}>``

LIST(또는 문자열)에서 COUNT 값을, OBJECT 에서 KEYS 에 키를 가지고 있는
항목들을 가져옵니다. COUNT 가 음수면 끝에서부터 가져옵니다.

.. code:: lisp

    pact> (take 2 "abcd")
    "ab"
    pact> (take (- 3) [1 2 3 4 5])
    [3 4 5]
    pact> (take ['name] { 'name: "Vlad", 'active: false})
    {"name": "Vlad"}

tx-hash
~~~~~~~

*→* ``string``

현재 트랜잭션의 해시를 문자열로 형태로 획득합니다.

.. code:: lisp

    pact> (tx-hash)
    "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"

typeof
~~~~~~

*x* ``<a>`` *→* ``string``

X 의 타입을 문자열 형태로 반환합니다.

.. code:: lisp

    pact> (typeof "hello")
    "string"

where
~~~~~

*field* ``string`` *app* ``(x:<a> -> bool)`` *value* ``object:<{row}>``
*→* ``bool``

‘filter’ 및 ‘select’에서 사용되는 유틸리티로서 VALUE 의 FIELD 에 APP 을
적용합니다.

.. code:: lisp

    pact> (filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
    [{"name": "Juan", "age": 15}]

yield
~~~~~

*OBJECT* ``object:<{y}>`` *→* ``object:<{y}>``

후속 Pact 스텝에서 ‘resume’와 함께 사용할 OBJECT를 양도합니다. 이 객체는
상위 수준만 ’resume’으로 바인딩이 가능한 점에서 데이터베이스 행 객체와
유사합니다. 중첩된 객체들은 오파크(opaque) 타입의 JSON 값으로
변환됩니다.

.. code:: lisp

    (yield { "amount": 100.0 })

.. _Database:

Database
--------

create-table
~~~~~~~~~~~~

*table* ``table:<{row}>`` *→* ``string``

테이블 TABLE 을 생성합니다.

.. code:: lisp

    (create-table accounts)

최상위 수준에서만 사용 가능: 모듈 코드에 적용 시 실패합니다.

describe-keyset
~~~~~~~~~~~~~~~

*keyset* ``string`` *→* ``value``

KEYSET 를 위한 메타데이터를 얻습니다.

최상위 수준에서만 사용 가능: 모듈 코드에 적용 시 실패합니다.

describe-module
~~~~~~~~~~~~~~~

*module* ``string`` *→* ``value``

MODULE 을 위한 메타데이터를 얻습니다. ‘name’, ‘hash’, ‘blessed’, ‘code’
및 ‘keyset’ 필드가 있는 객체를 반환합니다.

.. code:: lisp

    (describe-module 'my-module)

최상위 수준에서만 사용 가능: 모듈 코드에 적용 시 실패합니다.

describe-table
~~~~~~~~~~~~~~

*table* ``table:<{row}>`` *→* ``value``

TABLE 을 위한 메타데이터를 얻습니다. ‘name’, ‘hash’, ‘blessed’, ‘code’
및 ‘keyset’ 필드가 있는 객체를 반환합니다.

.. code:: lisp

    (describe-table accounts)

최상위 수준에서만 사용 가능: 모듈 코드에 적용 시 실패합니다.

insert
~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *object* ``object:<{row}>``
*→* ``string``

TABLE 에 OBJECT 의 KEY 에 대한 항목을 기록합니다. 해당 KEY 에 이미
데이터가 존재하는 경우에는 실패합니다.

.. code:: lisp

    (insert accounts "Alice" { "balance": 0.0, "note": "Created account." })

keylog
~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *txid* ``integer``
*→* ``[object]``

txid 로 인덱싱된 객체 리스트에서 TXID 당시 또는 이후의 트랜잭션의 KEY 에
대한 TABLE 업데이트를 반환합니다.

.. code:: lisp

    (keylog accounts "Alice" 123485945)

keys
~~~~

*table* ``table:<{row}>`` *→* ``[string]``

TABLE 에서 모든 키를 반환합니다.

.. code:: lisp

    (keys accounts)

read
~~~~

*table* ``table:<{row}>`` *key* ``string`` *→* ``object:<{row}>``

*table* ``table:<{row}>`` *key* ``string`` *columns* ``[string]``
*→* ``object:<{row}>``

TABLE 에서 KEY 의 행을 읽어 데이터베이스 레코드 객체를 반환하거나,
COLUMNS 가 지정된 경우에 지정된 열만 포함한 행을 읽어들입니다.

.. code:: lisp

    (read accounts id ['balance 'ccy])

select
~~~~~~

*table* ``table:<{row}>`` *where* ``(row:object:<{row}> -> bool)``
*→* ``[object:<{row}>]``

*table* ``table:<{row}>`` *columns* ``[string]``
*where* ``(row:object:<{row}> -> bool)`` *→* ``[object:<{row}>]``

테이블에서 WHERE 을 적용하여 전체 행 또는 COLUMNS 를 포함 여부를 부울
값으로 결정하여 select 합니다.

.. code:: lisp

    (select people ['firstName,'lastName] (where 'name (= "Fatima")))
    (select people (where 'age (> 30)))

txids
~~~~~

*table* ``table:<{row}>`` *txid* ``integer`` *→* ``[integer]``

TABLE 에서 TXID 값보다 크거나 같은 모든 txid 값을 반환합니다

.. code:: lisp

    (txids accounts 123849535)

txlog
~~~~~

*table* ``table:<{row}>`` *txid* ``integer`` *→* ``[value]``

트랜잭션 TXID 에서 수행된 TABLE 에 대한 모든 업데이트를 반환합니다.

.. code:: lisp

    (txlog accounts 123485945)

update
~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *object* ``object:<{row}>``
*→* ``string``

TABLE 에 OBJECT 의 KEY 에 대한 항목을 기록합니다. 해당 KEY 에 이미
데이터가 존재하는 경우에는 실패합니다.

.. code:: lisp

    (update accounts { "balance": (+ bal amount), "change": amount, "note": "credit" })

with-default-read
~~~~~~~~~~~~~~~~~

*table* ``table:<{row}>`` *key* ``string`` *defaults* ``object:<{row}>``
*bindings* ``binding:<{row}>`` *→* ``<a>``

TABLE 에서 KEY 에 대한 행을 읽어들이고 후속 바디 문에 대해 BINDING 별로
열을 바인딩하기 위한 특수한 형식입니다. 행이 발견되지 않으면 일치하는 키
이름을 가진 객체인 DEFAULTS 에서 열을 읽어들입니다.

.. code:: lisp

    (with-default-read accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
       (format "Balance for {} is {} {}" [id bal ccy]))

with-read
~~~~~~~~~

*table* ``table:<{row}>`` *key* ``string``
*bindings* ``binding:<{row}>`` *→* ``<a>``

TABLE 에서 KEY 에 대한 행을 읽어들이고 후속 바디 문에 대해 BINDING 별로
열을 바인딩하기 위한 특수 타입입니다.

.. code:: lisp

    (with-read accounts id { "balance":= bal, "ccy":= ccy }
       (format "Balance for {} is {} {}" [id bal ccy]))

write
~~~~~

*table* ``table:<{row}>`` *key* ``string`` *object* ``object:<{row}>``
*→* ``string``

TABLE 에 OBJECT 의 KEY 에 대한 항목을 기록합니다.

.. code:: lisp

    (write accounts { "balance": 100.0 })

.. _Time:

Time
----

add-time
~~~~~~~~

*time* ``time`` *seconds* ``decimal`` *→* ``time``

*time* ``time`` *seconds* ``integer`` *→* ``time``

TIME 에 SECONDS 를 추가합니다. SECONDS 는 정수 또는 10진수일 수
있습니다.

.. code:: lisp

    pact> (add-time (time "2016-07-22T12:00:00Z") 15)
    "2016-07-22T12:00:15Z"

days
~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N 일, ‘add-time’과 함께 사용됩니다.

.. code:: lisp

    pact> (add-time (time "2016-07-22T12:00:00Z") (days 1))
    "2016-07-23T12:00:00Z"

diff-time
~~~~~~~~~

*time1* ``time`` *time2* ``time`` *→* ``decimal``

TIME1 과 TIME2 간의 차이(초)를 계산합니다.

.. code:: lisp

    pact> (diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
    23400

format-time
~~~~~~~~~~~

*format* ``string`` *time* ``time`` *→* ``string``

FORMAT 을 사용해 TIME 을 포맷팅합니다. 지원되는 형식은 `“시간 형식”
문서 <pact-reference.html#time-formats>`__\ 를 참조하세요.

.. code:: lisp

    pact> (format-time "%F" (time "2016-07-22T12:00:00Z"))
    "2016-07-22"

hours
~~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N 시간, ‘add-time’과 함께 사용됩니다

.. code:: lisp

    pact> (add-time (time "2016-07-22T12:00:00Z") (hours 1))
    "2016-07-22T13:00:00Z"

minutes
~~~~~~~

*n* ``decimal`` *→* ``decimal``

*n* ``integer`` *→* ``decimal``

N 분, ‘add-time’과 함께 사용됩니다

.. code:: lisp

    pact> (add-time (time "2016-07-22T12:00:00Z") (minutes 1))
    "2016-07-22T12:01:00Z"

parse-time
~~~~~~~~~~

*format* ``string`` *utcval* ``string`` *→* ``time``

FORMAT 을 사용해 UTCVAL 의 시간을 구성합니다. 지원되는 형식은 `“시간
형식” 문서 <pact-reference.html#time-formats>`__\ 를 참조하세요.

.. code:: lisp

    pact> (parse-time "%F" "2016-09-12")
    "2016-09-12T00:00:00Z"

time
~~~~

*utcval* ``string`` *→* ``time``

ISO8601 타입(%Y-%m-%dT%H:%M:%SZ)을 사용해 UTCVAL 의 시간을 구성합니다.

.. code:: lisp

    pact> (time "2016-07-22T11:26:35Z")
    "2016-07-22T11:26:35Z"

.. _Operators:

연산자
------

.. _bangeq:

!=
~~

*x* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
*y* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
*→* ``bool``

X 가 Y 와 같지 않으면 true 입니다.

.. code:: lisp

    pact> (!= "hello" "goodbye")
    true

.. _star:

\*
~~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

X 와 Y 를 곱합니다.

.. code:: lisp

    pact> (* 0.5 10.0)
    5
    pact> (* 3 5)
    15

.. _plus:

\+
~~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

*x* ``<a[string,[<l>],object:<{o}>]>``
*y* ``<a[string,[<l>],object:<{o}>]>``
*→* ``<a[string,[<l>],object:<{o}>]>``

숫자를 추가하거나, 문자열/리스트를 결합하거나 객체를 병합합니다.

.. code:: lisp

    pact> (+ 1 2)
    3
    pact> (+ 5.0 0.5)
    5.5
    pact> (+ "every" "body")
    "everybody"
    pact> (+ [1 2] [3 4])
    [1 2 3 4]
    pact> (+ { "foo": 100 } { "foo": 1, "bar": 2 })
    {"bar": 2, "foo": 100}

.. _minus:

\-
~~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

X 를 부정하거나 X 에서 Y 를 뺍니다.

.. code:: lisp

    pact> (- 1.0)
    -1.0
    pact> (- 3 2)
    1

.. _slash:

/
~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

X 를 Y 로 나눕니다.

.. code:: lisp

    pact> (/ 10.0 2.0)
    5
    pact> (/ 8 3)
    2

.. _lt:

<
~

*x* ``<a[integer,decimal,string,time]>``
*y* ``<a[integer,decimal,string,time]>`` *→* ``bool``

X 가 Y 보다 작을 경우 true 입니다.

.. code:: lisp

    pact> (< 1 3)
    true
    pact> (< 5.24 2.52)
    false
    pact> (< "abc" "def")
    true

.. _lteq:

<=
~~

*x* ``<a[integer,decimal,string,time]>``
*y* ``<a[integer,decimal,string,time]>`` *→* ``bool``

X 가 Y 보다 작거나 같을 경우 true 입니다.

.. code:: lisp

    pact> (<= 1 3)
    true
    pact> (<= 5.24 2.52)
    false
    pact> (<= "abc" "def")
    true

.. _eq:

=
~

*x* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
*y* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>``
*→* ``bool``

X 가 Y 와 같을 경우 true 입니다.

.. code:: lisp

    pact> (= [1 2 3] [1 2 3])
    true
    pact> (= 'foo "foo")
    true
    pact> (= { 1: 2 } { 1: 2})
    true

.. _gt:

>
~

*x* ``<a[integer,decimal,string,time]>``
*y* ``<a[integer,decimal,string,time]>`` *→* ``bool``

X 가 Y 보다 클 경우 true 입니다.

.. code:: lisp

    pact> (> 1 3)
    false
    pact> (> 5.24 2.52)
    true
    pact> (> "abc" "def")
    false

.. _gteq:

>=
~~

*x* ``<a[integer,decimal,string,time]>``
*y* ``<a[integer,decimal,string,time]>`` *→* ``bool``

X 가 Y 와 크거나 같을 경우 true 입니다.

.. code:: lisp

    pact> (>= 1 3)
    false
    pact> (>= 5.24 2.52)
    true
    pact> (>= "abc" "def")
    false

.. _hat:

^
~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

X 의 Y 승을 구합니다.

.. code:: lisp

    pact> (^ 2 3)
    8

abs
~~~

*x* ``decimal`` *→* ``decimal``

*x* ``integer`` *→* ``integer``

X 의 절대 값입니다.

.. code:: lisp

    pact> (abs (- 10 23))
    13

and
~~~

*x* ``bool`` *y* ``bool`` *→* ``bool``

쇼트 서킷이 지원되는 부울 로직입니다.

.. code:: lisp

    pact> (and true false)
    false

and? {#and?}
~~~~~~~~~~~~

*a* ``(x:<r> -> bool)`` *b* ``(x:<r> -> bool)`` *value* ``<r>``
*→* ``bool``

A 와 B 에 VALUE 를 적용한 결과에 논리식 ’and’를 적용합니다. 쇼트 서킷을
지원합니다

.. code:: lisp

    pact> (and? (> 20) (> 10) 15)
    false

ceiling
~~~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

10진수 X 의 값을 정수로 반올림하거나 10진수 형태의 PREC 정밀도로
반올림합니다.

.. code:: lisp

    pact> (ceiling 3.5)
    4
    pact> (ceiling 100.15234 2)
    100.16

exp
~~~

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

X 의 거듭제곱입니다.

.. code:: lisp

    pact> (round (exp 3) 6)
    20.085537

floor
~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

10진수 X 의 값을 정수로 반내림하거나 10진수 형태의 PREC 정밀도로
반내림합니다.

.. code:: lisp

    pact> (floor 3.5)
    3
    pact> (floor 100.15234 2)
    100.15

ln
~~

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

X 의 자연 로그입니다.

.. code:: lisp

    pact> (round (ln 60) 6)
    4.094345

log
~~~

*x* ``<a[integer,decimal]>`` *y* ``<a[integer,decimal]>``
*→* ``<a[integer,decimal]>``

*x* ``<a[integer,decimal]>`` *y* ``<b[integer,decimal]>``
*→* ``decimal``

밑이 X 인 Y 의 로그입니다.

.. code:: lisp

    pact> (log 2 256)
    8

mod
~~~

*x* ``integer`` *y* ``integer`` *→* ``integer``

X 모듈로 Y 입니다.

.. code:: lisp

    pact> (mod 13 8)
    5

not
~~~

*x* ``bool`` *→* ``bool``

부울 로직입니다.

.. code:: lisp

    pact> (not (> 1 2))
    true

not? {#not?}
~~~~~~~~~~~~

*app* ``(x:<r> -> bool)`` *value* ``<r>`` *→* ``bool``

APP 에 VALUE 를 적용한 결과에 논리식 ’not’을 적용합니다.

.. code:: lisp

    pact> (not? (> 20) 15)
    false

or
~~

*x* ``bool`` *y* ``bool`` *→* ``bool``

쇼트 서킷이 지원되는 부울 로직입니다.

.. code:: lisp

    pact> (or true false)
    true

or? {#or?}
~~~~~~~~~~

*a* ``(x:<r> -> bool)`` *b* ``(x:<r> -> bool)`` *value* ``<r>``
*→* ``bool``

A 와 B 에 VALUE 를 적용한 결과에 논리식 ’or’을 적용합니다. 쇼트 서킷을
지원합니다.

.. code:: lisp

    pact> (or? (> 20) (> 10) 15)
    true

round
~~~~~

*x* ``decimal`` *prec* ``integer`` *→* ``decimal``

*x* ``decimal`` *→* ``integer``

뱅커 라운딩(Banker’s rounding) 기법으로 10진수 X 의 값을 정수로
반올림하거나 10진수 형태의 PREC 정밀도로 반올림합니다.

.. code:: lisp

    pact> (round 3.5)
    4
    pact> (round 100.15234 2)
    100.15

sqrt
~~~~

*x* ``<a[integer,decimal]>`` *→* ``<a[integer,decimal]>``

X 의 제곱근입니다.

.. code:: lisp

    pact> (sqrt 25)
    5

.. _Keysets:

Keysets
-------

define-keyset
~~~~~~~~~~~~~

*name* ``string`` *keyset* ``string`` *→* ``string``

KEYSET 에서 키셋을 NAME 으로 정의합니다. 키셋 NAME 이 이미 존재하는 경우
새 값으로 업데이트하기 전에 키셋이 enforce 됩니다.

.. code:: lisp

    (define-keyset 'admin-keyset (read-keyset "keyset"))

최상위 수준에서만 사용 가능: 모듈 코드에 적용 시 실패합니다.

enforce-keyset
~~~~~~~~~~~~~~

*keyset-or-name* ``<k[string,keyset]>`` *→* ``bool``

BODY 를 실행하기 전에 메시지 키에 대해 KEYSET-OR-NAME 을 enforce 하기
위한 특수 형식입니다. KEYSET-OR-NAME 는 키셋 이름 또는 키셋 객체의
기호가 될 수 있습니다.

.. code:: lisp

    (with-keyset 'admin-keyset ...)
    (with-keyset (read-keyset "keyset") ...)

keys-2
~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

키셋 Predicate 함수로서 키셋의 키와 적어도 2 개가 일치해야 합니다.

.. code:: lisp

    pact> (keys-2 3 1)
    false

keys-all
~~~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

키셋 Predicate 함수로서 키셋의 모든 키와 일치해야 합니다.

.. code:: lisp

    pact> (keys-all 3 3)
    true

keys-any
~~~~~~~~

*count* ``integer`` *matched* ``integer`` *→* ``bool``

키셋 Predicate 함수로서 키셋의 키와 적어도 1 개가 일치해야 합니다.

.. code:: lisp

    pact> (keys-any 10 1)
    true

read-keyset
~~~~~~~~~~~

*key* ``string`` *→* ``keyset``

키셋 형태({ “keys”: KEYLIST, “pred”: PREDFUN })의 메시지 데이터 바디에서
KEY 를 읽어들입니다. PREDFUN 은 키 predicate으로 귀결되어야 합니다.

.. code:: lisp

    (read-keyset "admin-keyset")

.. _repl-lib:

REPL-only functions
-------------------

다음 함수는 interactive REPL 또는 ``.repl`` 확장자가 포함된 스크립트
파일에서 마술처럼 로드됩니다. 이들은 블록체인 기반의 실행에서는 사용할
수 없습니다.

begin-tx
~~~~~~~~

*→* ``string``

*name* ``string`` *→* ``string``

선택적인 NAME 과 함께 트랜잭션을 시작합니다.

.. code:: lisp

    (begin-tx "load module")

bench
~~~~~

*exprs* ``*`` *→* ``string``

EXPRS 의 실행을 벤치마킹합니다.

.. code:: lisp

    (bench (+ 1 2))

commit-tx
~~~~~~~~~

*→* ``string``

트랜잭션을 수행합니다.

.. code:: lisp

    (commit-tx)

env-data
~~~~~~~~

*json* ``<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,value]>``
*→* ``string``

트랜잭션 JSON 데이터를 설정합니다(인코딩된 문자열 형태 또는 JSON 으로
강제 변환된 Pact 타입).

.. code:: lisp

    pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
    "Setting transaction data"

env-entity
~~~~~~~~~~

*→* ``string``

*entity* ``string`` *→* ``string``

환경 기밀 ENTITY ID 를 설정하거나 인수가 없는 경우에는 설정을
취소합니다. 이전의 모든 Pact 실행 상태를 해제합니다.

.. code:: lisp

    (env-entity "my-org")
    (env-entity)

env-gas
~~~~~~~

*→* ``integer``

*gas* ``integer`` *→* ``string``

가스 상태를 쿼리하거나 GAS 로 이를 설정합니다.

env-gaslimit
~~~~~~~~~~~~

*limit* ``integer`` *→* ``string``

환경의 가스 한도를 LIMIT 으로 설정합니다.

env-gasprice
~~~~~~~~~~~~

*price* ``decimal`` *→* ``string``

환경의 가스 가격을 PRICE 로 설정합니다.

env-gasrate
~~~~~~~~~~~

*rate* ``integer`` *→* ``string``

일정한 RATE 를 청구하도록 가스 모델을 업데이트합니다.

env-hash
~~~~~~~~

*hash* ``string`` *→* ``string``

현재 트랜잭션 해시를 설정합니다. HASH 는 유효한 BLAKE2b 512-비트
해시여야 합니다.

.. code:: lisp

    pact> (env-hash (hash "hello"))
    "Set tx hash to e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"

env-keys
~~~~~~~~

*keys* ``[string]`` *→* ``string``

트랜잭션 시그니처 KEYS 를 설정합니다.

.. code:: lisp

    pact> (env-keys ["my-key" "admin-key"])
    "Setting transaction keys"

env-step
~~~~~~~~

*→* ``string``

*step-idx* ``integer`` *→* ``string``

*step-idx* ``integer`` *rollback* ``bool`` *→* ``string``

*step-idx* ``integer`` *rollback* ``bool`` *resume* ``object:<{y}>``
*→* ``string``

Pact 스텝의 상태를 설정합니다. 인수가 없는 경우 스텝의 설정을
취소합니다. STEP-IDX 를 통해 실행한 스텝 인덱스를 설정합니다. ROLLBACK
은 롤백 표현식이 있는 경우 실행을 명령합니다. RESUME 는 ‘resume’를 통해
읽어들일 값을 설정합니다. 이전의 모든 Pact 실행 상태를 해제합니다.

.. code:: lisp

    (env-step 1)
    (env-step 0 true)

expect
~~~~~~

*doc* ``string`` *expected* ``<a>`` *actual* ``<a>`` *→* ``string``

ACTUAL 을 평가하고 EXPECTED 와 동일한지 검증합니다.

.. code:: lisp

    pact> (expect "Sanity prevails." 4 (+ 2 2))
    "Expect: success: Sanity prevails."

expect-failure
~~~~~~~~~~~~~~

*doc* ``string`` *exp* ``<a>`` *→* ``string``

EXP 를 평가하고 오류가 발생한 경우에만 계속 진행합니다.

.. code:: lisp

    pact> (expect-failure "Enforce fails on false" (enforce false "Expected error"))
    "Expect failure: success: Enforce fails on false"

json
~~~~

*exp* ``<a>`` *→* ``value``

Pact 표현식 EXP 를 JSON 값으로 인코딩합니다. Pact 값은 API 출력시
자동으로 JSON 로 표현되기 때문에 이 값은 테스트에서만 필요합니다.

.. code:: lisp

    pact> (json [{ "name": "joe", "age": 10 } {"name": "mary", "age": 25 }])
    [{"age":10,"name":"joe"},{"age":25,"name":"mary"}]

load
~~~~

*file* ``string`` *→* ``string``

*file* ``string`` *reset* ``bool`` *→* ``string``

FILE 을 로드 및 평가하며 선택적인 RESET 값이 true 인 경우 미리 REPL
상태를 재설정합니다.

.. code:: lisp

    (load "accounts.repl")

pact-state
~~~~~~~~~~

*→* ``object``

이전의 Pact 실행의 상태를 검사합니다. ‘yield’(양도 값), ‘false’(결과
없음), ‘step’(실행된 스텝), ‘executed’(엔터티가 일치하지 않아서 스텝을
건너 뛰었는지 여부 표시) 필드와 함께 객체를 반환합니다.

.. code:: lisp

    (pact-state)

print
~~~~~

*value* ``<a>`` *→* ``string``

VALUE 를 터미널에 quote 없이, 이스케이핑되지 않은 상태로 출력합니다.

rollback-tx
~~~~~~~~~~~

*→* ``string``

트랜잭션을 롤백합니다.

.. code:: lisp

    (rollback-tx)

sig-keyset
~~~~~~~~~~

*→* ``keyset``

편리한 함수로 ‘keys-all’을 predicate으로 사용하여 메시지 시그니처에
존재하는 키로부터 키셋을 구성합니다.

typecheck
~~~~~~~~~

*module* ``string`` *→* ``string``

*module* ``string`` *debug* ``bool`` *→* ``string``

MODULE 을 타입체킹하고 선택적으로 DEBUG 출력을 활성화합니다.

verify
~~~~~~

*module* ``string`` *→* ``string``

MODULE 을 검증하여 모든 속성들이 유지되는지 검사합니다.

|image1|

Pact 속성 검사 시스템
=====================

어떤 시스템인가요?
------------------

Pact에서는 스마트 컨트랙트 작성자가 Pact 프로그램의 속성 또는 사양을
표현하고 자동 검사할 수 있도록 하는 기능을 함께 제공합니다.

Pact 속성 검사 시스템은 스마트 컨트랙트 프로그래밍 세계에서 현재
사용되고 있는 혼란스럽고 불확실한 환경에 대응해서 나왔습니다. 오류에
취약한 스마트 컨트랙트 작성자에게 스마트 컨트랙트 공격에 악용될만한 모든
방법을 상상해 볼 것을 요구하는 대신에, 공식 확인 시 배경 지식 없이도
코드에 대한 공격이 불가하다는 것을 스스로 입증할 수 있도록 돕고
있습니다.

예를 들어 임의적이고 복잡한 Pact 프로그램의 경우, 컨트랙트의 “관리자”만
데이터베이스를 수정할 수 있음을 확실하게 증명하고 다른 모든 사용자에게는
컨트랙트의 로직에 따라 DB 에 대한 읽기 전용 액세스만 허용되도록 하고
싶을 수 있습니다. 이 기능을 이용하면 블록체인에 코드를 배포하기 전
*정적으로* 이러한 속성을 증명할 수 있습니다.

작성자가 조합한 입력으로 일반화하여 프로그램 동작을 검증하는 기존의 유닛
테스트와 비교하여 이러한 Pact 속성 검사 시스템은 *자동으로* 테스트 중인
코드를 모든 가능한 입력 및 실행 경로를 통해 검사합니다.

Pact는 이를 위해 작성자가 데이터베이스 테이블의 열에 대해 *스키마
불변식* 을 지정하고, 함수의 인수, 반환 값, 키셋 적용, 데이터베이스
액세스 및 ``enforce``\ 의 사용과 관련된 함수에 대한 *속성* 을 지정 및
증명할 수 있도록 허용하고 있습니다.

쉽게 말해서 Pact의 속성은 “컨트랙트”에 대한 개념에 해당되고(참고: 이는
“스마트 컨트랙트”과는 다름), Pact의 불변식은 공식 확인 절차에서 정제된
타입으로 나아가기 위한 간소화된 초기 스텝에 해당됩니다.

초기 릴리스에서는 아직 Pact 언어가 100% 지원되지 않아 속성 검사기 *자체*
의 구현이 아직 공식 확인되지 않았지만 이것이 첫 스텝입니다. 가능한 모든
Pact 프로그램에 대한 지원을 계속해서 넓혀 나아가 결국은 속성 검사기의
정확성을 입증하고, 시간이 지나면서 작성자가 스마트 컨트랙트에 대해 훨씬
복잡한 속성도 표현하도록 지원할 수 있을 것으로 기대됩니다.

속성과 스키마 불변식은 어떤 형태를 가지고 있습니까?
---------------------------------------------------

아래에는 실행 중인 Pact의 속성에 대한 예가 나와 있습니다. 속성과 더불어
속성에 해당되는 함수의 docstring 을 함께 선언했습니다. 이 함수는 키셋
적용의 구현을 다른 함수에 맡기고 있기 때문에 ``enforce-admin`` 구현
방법에 대해 고민할 필요가 없습니다. 속성은 블록체인에 제출된 트랜잭션이
성공적으로 실행된 경우에 트랜잭션이 ``admins``\ 라는 이름의 키셋을
지정할 수 있는 적절한 시그니처를 갖도록 선언합니다.

.. code:: lisp

    (defun read-account (id)
      @doc   "Read data for account ID"
      @model [(property (authorized-by 'admins))]

      (enforce-admin)
      (read 'accounts id ['balance 'ccy 'amount]))

Pact에서는 여러 개의 속성을 동시에 정의할 수 있기 때문에 하나의 속성을
묶을 수 있는 대괄호 집합이 있습니다.

.. code:: lisp

    [p1 p2 p3 ...]

다음으로 스미카 불변식의 예를 살펴보겠습니다. 다음과 같은 스키마를 가진
모든 테이블의 경우, 속성 검사기가 성공을 거두는 경우에는 가능한 모든
코드 경로에 항상 불변식(토큰 잔액이 0 을 넘는)이 유지됩니다.

.. code:: lisp

    (defschema tokens
      @doc   "token schema"
      @model [(invariant (> balance 0))]

      username:string
      balance:integer)

어떻게 작용합니까?
------------------

Pact의 속성 검사기는 SMT(Satisfiability Modulo Theories) 솔버에서 언어의
시맨틱스를 구현하고 프로그램에 대한 공식을 구축하여 해당 공식의 유효성을
테스트하는 방법으로 작동합니다. SMT 솔버는 일부 Pact 코드에 대해 제공된
명제를 조작할 수 있는 변수에는 어떤 값도 할당하지 않는다는 것을 증명할
수 있습니다. Pact는 현재 Microsoft 의 `Z3 정리
증명 <https://github.com/Z3Prover/z3/wiki>`__\ 를 사용해 속성 검사
시스템을 지원하고 있습니다.

이러한 공식은 Pact 모듈의 함수, 이들 함수에 제공된 속성, 모듈의 스키마에
대해 선언된 불변식을 조합해 만들어졌습니다.

Pact 모듈의 모든 함수 정의에서는 다른 함수에 대한 모든 후속 호출이
인라이닝됩니다. 속성을 테스트할 수 있으려면 먼저 이렇게 인라이닝된
코드가 타입체킹을을 통과해야 합니다.

스키마 불변식의 경우, 속성 검사기가 귀납적 접근 방식을 채택하여 스키마
불변식이 현재 데이터베이스의 데이터에 대해 *유지* 된다고 가정하고,
모듈의 모든 함수가 가능한 모든 DB 수정에 대해 이러한 불변식을 유지하는지
*검사* 합니다.

어떻게 사용하나요?
------------------

모듈에서 원하는 모든 불변식 및 속성 주석을 제공한 후에는 ``verify``\ 를
호출하여 속성 검사를 실행합니다.

.. code:: lisp

    (verify 'module-name)

이렇게 하면 코드를 타입체킹 후 통과 할 경우 모든 불변식과 속성을 검사할
수 있습니다.

속성 표현
---------

인수, 반환 값, 표준 산술 및 비교 연산자
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

속성에서는 이름으로 직접 함수 인수를 참조할 수 있으며, 반환 값을
``result``\ 라는 이름으로 참조할 수 있습니다.

.. code:: lisp

    (defun negate:integer (x:integer)
      @doc   "negate a number"
      @model [(property (= result (* -1 x)))]

      (* x -1))

여기에서 정수 및 10진수에 대한 표준 산술 연산자는 일반적인 Pact
코드에서처럼 작동한다는 것을 알 수 있습니다.

또한 표준 비교 연산자를 사용하여 속성을 정의할 수도 있습니다.

.. code:: lisp

    (defun abs:integer (x:integer)
      @doc   "absolute value"
      @model [(property (>= result 0))]

      (if (< x 0)
        (negate x)
        x))

부울 연산자
~~~~~~~~~~~

표준 부울 연산자인 ``and``, ``or``, 및 ``not``\ 외에도 Pact의 속성 검사
언어는 ``when`` 형태의 논리적 함의를 지원하는데, 여기에서
``(when x y)``\ 는 ``(or (not x) y)``\ 에 상응합니다. 아래에서 세 가지
속성을 한꺼번에 정의해 보겠습니다.

.. code:: lisp

    (defun negate:integer (x:integer)
      @doc   "negate a number"
      @model
        [(property (when (< x 0) (> result 0)))
         (property (when (> x 0) (< result 0)))
         (property (and
           (when (< x 0) (> result 0))
           (when (> x 0) (< result 0))))]

      (* x -1))

트랜잭션 중단 및 성공
~~~~~~~~~~~~~~~~~~~~~

기본적으로 모든 속성은 테스트 중인 함수의 호출을 포함하여 트랜잭션이
성공적으로 완료될 때 예측이 됩니다. 따라서 아래와 같은 속성은

.. code:: lisp

    (defun ensured-positive (val:integer)
      @doc   "halts when passed a non-positive number"
      @model [(property (!= result 0))]

      (enforce (> val 0) "val is not positive")
      val)

는 ``enforce``\ 를 사용하기 때문에 통과합니다.

블록체인에서 런타임 시 ``enforce`` 호출이 실패하면 이것이 포함된
트랜잭션이 중단됩니다. ``properties``\ 는 성공적인 트랜잭션에만 관심이
있기 때문에 각 enforce 호출을 통과하기 위해 필요한 조건은
가정되어있습니다.

보다 포괄적인 속성 API 설명서
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

속성에서 사용할 수 있는 모든 기능 리스트는 `속성 및 불변식
함수 <http://pact-language.readthedocs.io/en/latest/pact-properties-api.html>`__\ 의
API 설명서를 참조하세요.

스키마 불변식 표현
------------------

스키마 불변식은 속성 정의에 사용할 수 있는 기능에 대해 엄격하게 제한된
부분 집합으로 설명이 됩니다. 즉, 인증, DB 액세스, 트랜잭션 성공/실패,
함수 인수 및 반환 값과 관련이 없는 함수입니다. 불변식 정의에 사용할 수
있는 모든 함수 리스트는 `속성 및 불변식
함수 <http://pact-language.readthedocs.io/en/latest/pact-properties-api.html>`__\ 의
API 설명서를 참조하세요.

.. raw:: html

   <!--- *** This second is disabled until we add `valid`/`satisfiable` alternatives to `property`, which currently assumes tx success ***

   ### Valid, satisfiable, and explicit transaction abort/success

   TODO: more. talk about valid, satisfiable, and the lack of the default
   success condition of property.

   Pact's property language supports the notions of `success` and `abort` to
   describe whether programs will successfully run to completion within a
   transaction on the blockchain:

   ```
   (defun failure-guaranteed:bool ()
     ("always fails" (valid abort))
     (enforce false "cannot pass"))
   ```

   TODO: more

   -->

키셋 인증
~~~~~~~~~

Pact에서는 키를 사전 정의된 이름(\ ``define-keyset``\ 로 정의)으로
참조하거나 값 형태로 배포할 수 있습니다. 속성 검사 시스템은 두 가지
스타일의 키셋 사용을 모두 지원합니다.

이름이 부여된 키셋의 경우, 가능한 모든 코드 경로에 키셋이 enforce 되는
경우에만 ``authorized-by`` 속성이 유지됩니다.

.. code:: lisp

    (defun admins-only (action:string)
      @doc   "Only admins or super-admins can call this function successfully.
      @model
        [(property (or (authorized-by 'admins) (authorized-by 'super-admins)))
         (property (when (== "create" action) (authorized-by 'super-admins)))]

      (if (== action "create")
        (create)
        (if (== action "update")
          (update)
          (incorrect-action action))))

Row-level 키셋 적용의 일반적인 패턴에서는 테이블이 각 사용자에 대해
하나의 행을 가지고 있고 각 사용자의 행에는 행 수정 시 권한이 부여된
키셋이 포함되어 있기 때문에 ``row-enforced`` 속성을 사용해 이 패턴이
올바르게 구현되었는지 확인할 수 있습니다.

다음 속성이 테스트를 통과하려면 코드가 변수 ``name``\ 에 의해 값이
입력된 행에서 ``accounts`` 테이블의 ``ks`` 열에 저장된 키셋을 추출하고,
``enforce-keyset``\ 를 사용해 이를 적용해야 합니다.

.. code:: lisp

    (row-enforced 'accounts 'ks name)

실행 중인 ``row-enforced``\ 에 대한 몇 가지 예는 “간단한 잔액 이체 예”와
아래의 “전체 및 존재 정량화” 섹션을 참조하세요.

데이터베이스 액세스
~~~~~~~~~~~~~~~~~~~

데이터베이스 테이블 액세스를 묘사하기 위해 속성 언어는 다음 속성을
가지고 있습니다.

-  ``(table-written 'accounts)`` - that any cell of the table
   ``accounts`` is written
-  ``(table-read 'accounts)`` - that any cell of the table ``accounts``
   is read
-  ``(row-written 'accounts k)`` - that the row keyed by the variable
   ``k`` is written
-  ``(row-read 'accounts k)`` - that the row keyed by the variable ``k``
   is read

자세한 내용은 아래의 “전체 및 존재 정량화”에 나온 예를 참조하세요.

질량 보존 및 열 델타
~~~~~~~~~~~~~~~~~~~~

열의 값 총계를 트랜잭션 전과 후에 동일하게 유지하는 것이 바람직한 경우가
있습니다. 달리 말하자면, 트랜잭션이 끝났을 때 열에 대한 모든 업데이트
합이 0 이 되도록 하는 것입니다. 이 패턴을 포착하기 위해 테이블 및 열
이름을 가져오는 ``conserves-mass``\ 속성을 사용하고 있습니다.

.. code:: lisp

    (conserves-mass 'accounts 'balance)

이 속성을 사용하는 예는 아래의 “간단한 잔액 이체 예”를 참조하세요

``conserves-mass``\ 는 실제로 ``column-delta``\ 라고 불리는 또 다른
속성이 극히 일부 적용된 것에 불과하며, 트랜잭션 동안 열에 대한 모든
변경의 합에 대한 수치 값을 반환하는 것으로 밝혀졌습니다. 따라서
``(conserves-mass 'accounts 'balance)`` 는 실제로 다음과 똑같습니다.

.. code:: lisp

    (= 0 (column-delta 'accounts 'balance))

또한 ``column-delta``\ 을 사용하면 열이 단조 증가하거나

.. code:: lisp

    (>= 0 (column-delta 'accounts 'balance))

트랜잭션 동안 정해진 금액까지 증가하도록 보장할 수 있습니다.

.. code:: lisp

    (= 1 (column-delta 'accounts 'balance))

``column-delta``\ 는 트랜잭션 전부터 후까지 정해진 금액까지 증가하도록
정의되었습니다.(예: ``after - before``) – 변경의 절대 값이 아님. 따라서
여기에서 ``1``\ 은 ``1``\ 이 열의 총계까지 증가한다는 것을 의미합니다.

보편적 및 존재적 정량화 (Universal and existential quantification)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

위의 ``(row-enforced 'accounts 'ks key)`` 또는
``(row-written 'accounts key)`` 같은 예에서는 오직 ``key``\ 라는 이름의
변수를 사용해서 함수 인수를 참조했습니다. 하지만 함수에서 단일 행만을
업데이트 하지 않는 경우 기록될 모든 행에 대해 적용하고 싶다면 어떨까요?

이러한 상황에서는 ``모든`` 행에 대해 적용하기 위해 보편적 정량화를
사용할 수 있습니다.

.. code:: lisp

    (property
      (forall (key:string)
       (when (row-written 'accounts key)
         (row-enforced 'accounts 'ks key))))

이 속성은 해당 함수에 의해 쓰여지기 가능한 모든 행에서 열 ``ks``\ 의
키셋을 해당 행에 대해 enforce 해야 한다는 것을 말해줍니다.

마찬가지로 가능한 모든 키에 대해 정량화를 수행하는 대신에 트랜잭션 동안
읽어들인 행이 단순히 존재한다는 것을 명시하고 싶은 경우에는 다음과 같이
존재적 정량화를 사용할 수 있습니다.

.. code:: lisp

    (property
      (exists (key:string)
        (row-read 'accounts key)))

보편적 및 존재적 정량화를 모두 원할 경우에는 타입 주석이 필요합니다.

속성 정의 및 재사용
~~~~~~~~~~~~~~~~~~~

``defproperty``\ 를 사용하여 모듈 수준에서 속성을 정의할 수 있습니다.

.. code:: lisp

    (defmodule accounts
      @model
        [(defproperty conserves-mass
           (= (column-delta 'accounts 'balance) 0.0))
         (defproperty auth-required
           (authorized-by 'accounts-admin-keyset))]

      ; ...
      )

그리고 속성의 이름을 참조하여 함수 수준에서 속성을 사용할 수 있습니다.

.. code:: lisp

    (defun read-account (id)
      @model [(property auth-required)]

      ; ...
      )

간단한 잔액 이체 예제
---------------------

예제로 아래의 테이블에의 두 계정에서 일부 잔액을 이체하는 함수를 작성해
보겠습니다.

.. code:: lisp

    (defschema account
      @doc "user accounts with balances"

      balance:integer
      ks:keyset)

    (deftable accounts:{account})

두 계정 간의 잔액 이체를 위한 다음 코드는 처음 보기에는 문제가 없어
보이지만, 사실상 많은 버그가 존재합니다. 이러한 버그는 다른 속성의
도움을 받아서, 그리고 테이블에 불변식을 추가하여 퇴치가 가능합니다.

.. code:: lisp

    (defun transfer (from:string to:string amount:integer)
      @doc   "Transfer money between accounts"
      @model [(property (row-enforced 'accounts 'ks from))]

      (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
        (with-read accounts to { 'balance := to-bal }
          (enforce-keyset from-ks)
          (enforce (>= from-bal amount) "Insufficient Funds")
          (update accounts from { "balance": (- from-bal amount) })
          (update accounts to   { "balance": (+ to-bal amount) }))))

잔액이 0 미만으로 떨어지지 않도록 불변식을 추가해 보겠습니다.

.. code:: lisp

    (defschema account
      @doc   "user accounts with balances"
      @model [(invariant (>= balance 0))]

      balance:integer
      ks:keyset)

이제 ``verify``\ 를 사용해 이 모듈의 모든 속성을 검증할 때 Pact의 속성
검사기가 ``amount``\ 에 ``-1``\ 을 적용하는 방식으로 양수인 잔액
불변식을 조작할 수 있음을 발견합니다(잔액이 ``0``\ 일 경우). 이 경우에는
실제로 “발신자”가 음수인 액수를 이체하여 다른 사람으로부터 돈을 강탈할
수 있습니다. ``(> amount 0)``\ 를 적용하여 이 문제를 해결하고 다시
시도해 보겠습니다.

.. code:: lisp

    (defun transfer (from:string to:string amount:integer)
      @doc   "Transfer money between accounts"
      @model [(property (row-enforced 'accounts 'ks from))]

      (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
        (with-read accounts to { 'balance := to-bal }
          (enforce-keyset from-ks)
          (enforce (>= from-bal amount) "Insufficient Funds")
          (enforce (> amount 0)         "Non-positive amount")
          (update accounts from { "balance": (- from-bal amount) })
          (update accounts to   { "balance": (+ to-bal amount) }))))

이 시점에서 속성 검사기가 코드의 유효성을 검사하지만, 해당 함수가 토큰의
생성 또는 훼손을 할 수 없도록 또 다른 속성인
``(conserves-mass 'accounts 'balance)``\ 를 추가해 보겠습니다.

.. code:: lisp

    (defun transfer (from:string to:string amount:integer)
      @doc   "Transfer money between accounts"
      @model
        [(property (row-enforced 'accounts 'ks from))
         (property (conserves-mass 'accounts 'balance))]

      (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
        (with-read accounts to { 'balance := to-bal }
          (enforce-keyset from-ks)
          (enforce (>= from-bal amount) "Insufficient Funds")
          (enforce (> amount 0)         "Non-positive amount")
          (update accounts from { "balance": (- from-bal amount) })
          (update accounts to   { "balance": (+ to-bal amount) }))))

이때 ``verify``\ 를 실행하면 속성 검사기가 다시 버그를 찾아냅니다.
``from`` 및 ``to``\ 가 같은 계정으로 설정이 되면 속성 조작이 가능합니다.
이런 일이 발생하면 코드는 실제로 갑자기 토큰을 생성합니다.

그 방법을 알아보기 위해 두 가지 ``update`` 호출에 초점을 맞춰보겠습니다.
여기에서 ``from``\ 과 ``to``\ 는 같은 값으로 설정이 되어 있고,
``from-bal`` 및 ``to-bal`` 또한 ``previous-balance``\ 라는 값으로
설정되어 있습니다.

.. code:: lisp

    (update accounts "alice" { "balance": (- previous-balance amount) })
    (update accounts "alice" { "balance": (+ previous-balance amount) })

이 시나리오에서 두 번째 ``update`` 호출은 첫 번째 호출을 완벽하게
덮어쓰기 합니다(로). Alice 는 무료로 ``amount`` 토큰을 생성했습니다!

이렇게 의도되지 않은 동작을 막기 위해 또 다른 ``enforce``\ 를 (with
``(!= from to)``)와 함께 추가하여 문제를 수정합니다.

.. code:: lisp

    (defun transfer (from:string to:string amount:integer)
      @doc   "Transfer money between accounts"
      @model
        [(property (row-enforced 'accounts 'ks from))
         (property (conserves-mass 'accounts 'balance))]

      (with-read accounts from { 'balance := from-bal, 'ks := from-ks }
        (with-read accounts to { 'balance := to-bal }
          (enforce-keyset from-ks)
          (enforce (>= from-bal amount) "Insufficient Funds")
          (enforce (> amount 0)         "Non-positive amount")
          (enforce (!= from to)         "Sender is the recipient")
          (update accounts from { "balance": (- from-bal amount) })
          (update accounts to   { "balance": (+ to-bal amount) }))))

최종적으로 속성 검사기는 다음이 모두 true 인지 확인합니다.

-  발신자에게 반드시 이체 권한이 부여되었음
-  잔액이 0 아래로 떨어지는 것이 불가능함
-  토큰의 생성 또는 훼손이 불가능함

.. _properties-and-invariants:

속성 및 불변식 함수
===================

이들은 속성 및 불변식에서 사용할 수 있는 함수들로, 실행 가능한 Pact
코드에서 반드시 필요한 것은 아닙니다. 이들 함수는 속성에서는 모두 사용이
가능하지만, 불변식에서는 오직 일부만 사용 가능합니다. 일반적으로
불변식은 데이터의 형태를 설명하기 위한 용어를 가지고 있는 반면에, 속성은
함수 입력 및 출력과 데이터베이스 상호 작용을 설명하기 위한 용어를
더합니다. 또한 각 함수는 단순히 속성에서만 사용 가능한지, 아니면
불변식에서도 사용이 가능한지 명시합니다.

.. _Numerical:

수치 연산자
-----------

.. _FAddition:

\+
~~

.. code:: lisp

    (+ x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수 더하기.

불변식 및 속성에서 지원됩니다.

.. _FSubtraction:

\-
~~

.. code:: lisp

    (- x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수 빼기.

불변식 및 속성에서 지원됩니다.

.. _FMultiplication:

\*
~~

.. code:: lisp

    (* x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수 곱하기.

불변식 및 속성에서 지원됩니다.

.. _FDivision:

/
~

.. code:: lisp

    (/ x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수 나누기.

불변식 및 속성에서 지원됩니다.

.. _FExponentiation:

^
~

.. code:: lisp

    (^ x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수 거듭제곱.

불변식 및 속성에서 지원됩니다.

.. _FLogarithm:

log
~~~

.. code:: lisp

    (log b x)

-  ``b``\ 는 *a*\ 의 타입을 가집니다.
-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

밑이 ``b``\ 인 ``x``\ 의 로그.

불변식 및 속성에서 지원됩니다.

.. _FNumericNegation:

\-
~~

.. code:: lisp

    (- x)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수의 부정.

불변식 및 속성에서 지원됩니다.

.. _FSquareRoot:

sqrt
~~~~

.. code:: lisp

    (sqrt x)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수 제곱근.

불변식 및 속성에서 지원됩니다.

.. _FNaturalLogarithm:

ln
~~

.. code:: lisp

    (ln x)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

밑이 e 인 정수 및 10진수의 로그.

불변식 및 속성에서 지원됩니다.

.. _FExponential:

exp
~~~

.. code:: lisp

    (exp x)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수의 지수. 정수 또는 10진수 ``x``\ 의 e 승입니다.

불변식 및 속성에서 지원됩니다.

.. _FAbsoluteValue:

abs
~~~

.. code:: lisp

    (abs x)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  *a*\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

정수 및 10진수의 절대 값.

불변식 및 속성에서 지원됩니다.

.. _FBankersRound:

round
~~~~~

.. code:: lisp

    (round x)

-  ``x``\ 는 ``decimal``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

.. code:: lisp

    (round x prec)

-  ``x``\ 는 ``decimal``\ 의 타입을 가집니다.
-  ``prec``\ 는 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

뱅커 라운딩(Banker’s rounding) 기법으로 10진수를 ``x`` 정수로
반올림하거나 10진수 형태의 ``prec`` 정밀도로 반올림합니다.

불변식 및 속성에서 지원됩니다.

.. _FCeilingRound:

ceiling
~~~~~~~

.. code:: lisp

    (ceiling x)

-  ``x``\ 는 ``decimal``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

.. code:: lisp

    (ceiling x prec)

-  ``x``\ 는 ``decimal``\ 의 타입을 가집니다.
-  ``prec``\ 는 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

10진수를 ``x`` 다음 정수로 반올림하거나 10진수 형태의 ``prec`` 정밀도로
반올림합니다.

불변식 및 속성에서 지원됩니다.

.. _FFloorRound:

floor
~~~~~

.. code:: lisp

    (floor x)

-  ``x``\ 는 ``decimal``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

.. code:: lisp

    (floor x prec)

-  ``x``\ 는 ``decimal``\ 의 타입을 가집니다.
-  ``prec``\ 는 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

10진수를 ``x`` 이전 정수로 반내림하거나 10진수 형태의 ``prec`` 정밀도로
반내림합니다.

불변식 및 속성에서 지원됩니다.

.. _FModulus:

mod
~~~

.. code:: lisp

    (mod x y)

-  ``x``\ 는 ``string``\ 의 타입을 가집니다.
-  ``y``\ 는 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

정수 모듈러 연산

불변식 및 속성에서 지원됩니다.

.. _Logical:

논리 연산자
-----------

.. _FGreaterThan:

>
~

.. code:: lisp

    (> x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

``x`` > ``y``\ 일 경우 true 입니다.

불변식 및 속성에서 지원됩니다.

.. _FLessThan:

<
~

.. code:: lisp

    (< x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

``x`` < ``y``\ 일 경우 true 입니다.

불변식 및 속성에서 지원됩니다.

.. _FGreaterThanOrEqual:

>=
~~

.. code:: lisp

    (>= x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

``x`` >= ``y``\ 일 경우 true 입니다.

불변식 및 속성에서 지원됩니다.

.. _FLessThanOrEqual:

<=
~~

.. code:: lisp

    (<= x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

``x`` <= ``y``\ 일 경우 true 입니다.

불변식 및 속성에서 지원됩니다.

.. _FEquality:

=
~

.. code:: lisp

    (= x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``integer``, ``decimal``, ``string``, ``time``, ``bool``,
   ``object``, 또는 ``keyset``\ 의 타입을 가집니다.

``x`` = ``y``\ 일 경우 true 입니다.

불변식 및 속성에서 지원됩니다.

.. _FInequality:

!=
~~

.. code:: lisp

    (!= x y)

-  ``x``\ 는 *a*\ 의 타입을 가집니다.
-  ``y``\ 는 *a*\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``integer``, ``decimal``, ``string``, ``time``, ``bool``,
   ``object``, 또는 ``keyset``\ 의 타입을 가집니다.

``x`` != ``y``\ 일 경우 true 입니다.

불변식 및 속성에서 지원됩니다.

.. _FLogicalConjunction:

and
~~~

.. code:: lisp

    (and x y)

-  ``x``\ 는 ``bool``\ 의 타입을 가집니다.
-  ``y``\ 는 ``bool``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.

논리곱 쇼트 서킷

불변식 및 속성에서 지원됩니다.

.. _FLogicalDisjunction:

or
~~

.. code:: lisp

    (or x y)

-  ``x``\ 는 ``bool``\ 의 타입을 가집니다.
-  ``y``\ 는 ``bool``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.

논리합 쇼트 서킷

불변식 및 속성에서 지원됩니다.

.. _FLogicalNegation:

not
~~~

.. code:: lisp

    (not x)

-  ``x``\ 는 ``bool``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.

논리 부정

불변식 및 속성에서 지원됩니다.

.. _FLogicalImplication:

when
~~~~

.. code:: lisp

    (when x y)

-  ``x``\ 는 ``bool``\ 의 타입을 가집니다.
-  ``y``\ 는 ``bool``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.

논리적 함의.\ ``(or (not x) y)``\ 에 상응합니다.

불변식 및 속성에서 지원됩니다.

.. _Object:

객체 연사자
-----------

.. _FObjectProjection:

at
~~

.. code:: lisp

    (at k o)

-  ``k``\ 는\ ``string``\ 의 타입을 가집니다.
-  ``o``\ 는 ``object``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.

객체내 키 존재 확인.

불변식 및 속성에서 지원됩니다.

.. _FObjectMerge:

\+
~~

.. code:: lisp

    (+ x y)

-  ``x``\ 는 ``object``\ 의 타입을 가집니다.
-  ``y``\ 는 ``object``\ 의 타입을 가집니다.
-  ``object``\ 의 타입을 가진 값을 산출합니다.

객체 병합.

불변식 및 속성에서 지원됩니다.

.. _String:

문자열 연산자
-------------

.. _FStringLength:

length
~~~~~~

.. code:: lisp

    (length s)

-  ``s``\ 는 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

문자열 길이

불변식 및 속성에서 지원됩니다.

.. _FStringConcatenation:

\+
~~

.. code:: lisp

    (+ s t)

-  ``s``\ 는 ``string``\ 의 타입을 가집니다.
-  ``t``\ 는 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

문자열 결합

불변식 및 속성에서 지원됩니다.

.. _FStringToInteger:

str-to-int
~~~~~~~~~~

.. code:: lisp

    (str-to-int s)

-  ``s``\ 는 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

.. code:: lisp

    (str-to-int b s)

-  ``b``\ 는 ``integer``\ 의 타입을 가집니다.
-  ``s``\ 는 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.

문자열 정수 변환.

불변식 및 속성에서 지원됩니다.

.. _Temporal:

시간 연산자
-----------

.. _FTemporalAddition:

add-time
~~~~~~~~

.. code:: lisp

    (add-time t s)

-  ``t``\ 은 ``time``\ 의 타입을 가집니다.
-  ``s``\ 는 *a* 의 타입을 가집니다.
-  ``time``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는\ ``string`` 또는 ``decimal``\ 의 타입을 가집니다.

시간에 초 추가.

불변식 및 속성에서 지원됩니다.

.. _Quantification:

정량화 연산자
-------------

.. _FUniversalQuantification:

forall
~~~~~~

.. code:: lisp

    (forall (x:string) y)

-  *a*\ 의 타입을 가진 ``x``\ 를 바인딩합니다.
-  ``y``\ 는 *r* 의 타입을 가집니다.
-  *r* 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 *any type* 의 타입을 가집니다.
-  이때 *r* 은 *any type* 의 타입을 가집니다.

전체 정량화된 변수 바인딩.

속성에서만 지원됩니다.

.. _FExistentialQuantification:

exists
~~~~~~

.. code:: lisp

    (exists (x:string) y)

-  *a*\ 의 타입을 가진 ``x``\ 를 바인딩합니다.
-  ``y``\ 는 *r* 의 타입을 가집니다.
-  *r* 의 타입을 가진 값을 산출합니다.
-  이때 *r* 은 *any type* 의 타입을 가집니다.

존재 정량화된 변수 바인딩.

속성에서만 지원됩니다.

.. _FColumnOf:

column-of
~~~~~~~~~

.. code:: lisp

    (column-of t)

-  ``t``\ 는 ``table``\ 의 타입을 가집니다.
-  ``type``\ 의 타입을 가진 값을 산출합니다.

지정 ``table`` ``column``\ 의 *type*. 주로 정량화 연산자와 와 함꼐
쓰입니다. 예:
``(exists (col:(column-of accounts)) (column-written accounts col))``.

속성에서만 지원됩니다.

.. _Transactional:

트랜잭션 연산자
---------------

.. _FTransactionAborts:

abort
~~~~~

.. code:: lisp

    abort

-  ``bool``\ 의 타입을 가집니다.

트랜잭션 중단 여부입니다. 이 함수는 트랜잭션 성공을 가정하지 않는 명제를
표현할 때만 유용합니다. ``property``\ 를 통해 정의된 명제는 트랜잭션이
성공한다고 암묵적으로 가정합니다. 향후에 이 기능이 사용되는 모드를 새로
추가할 계획입니다. 이 기능이 필요하면 알려주시기 바랍니다.

속성에서만 지원됩니다.

.. _FTransactionSucceeds:

success
~~~~~~~

.. code:: lisp

    success

-  ``bool``\ 의 타입을 가집니다.

트랜잭션 성공 여부입니다. 이 함수는 트랜잭션 성공을 가정하지 않는 명제를
표현할 때만 유용합니다. ``property``\ 를 통해 정의된 명제는 트랜잭션이
성공한다고 암묵적으로 가정합니다. 향후에 이 기능이 사용되는 모드를 새로
추가할 계획입니다. 이 기능이 필요하면 알려주시기 바랍니다.

속성에서만 지원됩니다.

.. _FFunctionResult:

result
~~~~~~

.. code:: lisp

    result

-  *r* 의 타입을 가집니다.
-  이때 *r* 은 *any type* 의 타입을 가집니다.

테스트 중인 함수의 반환 값.

속성에서만 지원됩니다.

.. _Database:

데이터베이스 연산자
-------------------

.. _FTableWritten:

table-written
~~~~~~~~~~~~~

.. code:: lisp

    (table-written t)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.

해당 함수에서 테이블에 대한 쓰기 작업이 수행되는지의 여부.

속성에서만 지원됩니다.

.. _FTableRead:

table-read
~~~~~~~~~~

.. code:: lisp

    (table-read t)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.

해당 함수에서 테이블에 대한 읽기 작업이 수행되는지 여부.

속성에서만 지원됩니다.

.. _FCellDelta:

cell-delta
~~~~~~~~~~

.. code:: lisp

    (cell-delta t c r)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``c``\ 는 *b* 의 타입을 가집니다.
-  ``r``\ 은 ``string``\ 의 타입을 가집니다.
-  *c* 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.
-  이때 *b* 는 ``column`` 또는 ``string``\ 의 타입을 가집니다.
-  이때 *c* 는 ``integer`` 또는 ``decimal``\ 의 타입을 가집니다.

트랜잭션 전과 후의 셀 값 차이.

속성에서만 지원됩니다.

.. _FColumnDelta:

column-delta
~~~~~~~~~~~~

.. code:: lisp

    (column-delta t c)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``c``\ 는 *b* 의 타입을 가집니다.
-  *c* 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.
-  이떄 *b* 는 ``column`` 또는 ``string``\ 의 타입을 가집니다.
-  이떄 *c* is of type ``integer`` 또는 ``decimal``\ 의 타입을 가집니다.

트랜잭션 전과 후의 열 총합 값 차이.

속성에서만 지원됩니다.

.. _FColumnWritten:

column-written
~~~~~~~~~~~~~~

.. code:: lisp

    (column-written t c)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``c``\ 는 *b* 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.
-  이때 *b* 는 ``column`` 또는 ``string``\ 의 타입을 가집니다.

해당 함수에서 테이블의 열에 대한 쓰기 작업이 수행되는지의 여부.

속성에서만 지원됩니다.

.. _FColumnRead:

column-read
~~~~~~~~~~~

.. code:: lisp

    (column-read t c)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``c``\ 는 *b* 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.
-  이때 *b* 는 ``column`` 또는 ``string``\ 의 타입을 가집니다.

해당 함수에서 테이블의 열에 대한 읽기 작업이 수행되는지의 여부.

속성에서만 지원됩니다.

.. _FRowRead:

row-read
~~~~~~~~

.. code:: lisp

    (row-read t r)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``r``\ 은 ``string``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.

해당 함수에서 테이블의 행에 대한 읽기 작업이 수행되는지의 여부.

속성에서만 지원됩니다.

.. _FRowWritten:

row-written
~~~~~~~~~~~

.. code:: lisp

    (row-written t r)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``r``\ 은 ``string``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.

해당 함수에서 테이블의 행에 대한 쓰기 작업이 수행되는지의 여부.

속성에서만 지원됩니다.

.. _FRowReadCount:

row-read-count
~~~~~~~~~~~~~~

.. code:: lisp

    (row-read-count t r)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``r``\ 은 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.

트랜잭션 중에 행에서 읽기 작업이 수행된 횟수.

속성에서만 지원됩니다.

.. _FRowWriteCount:

row-write-count
~~~~~~~~~~~~~~~

.. code:: lisp

    (row-write-count t r)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``r``\ 은 ``string``\ 의 타입을 가집니다.
-  ``string``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.

트랜잭션 중에 행에서 쓰기 작업이 수행된 횟수.

속성에서만 지원됩니다.

.. _FRowExists:

row-exists
~~~~~~~~~~

.. code:: lisp

    (row-exists t r time)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``r``\ 은 ``string``\ 의 타입을 가집니다.
-  ``time``\ 은 {“before”, “after”} 중 하나의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.

트랜잭션 전 또는 후에 행이 존재하는지에 대한 여부.

속성에서만 지원됩니다.

.. _FPropRead:

read
~~~~

.. code:: lisp

    (read t r)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``r``\ 은 ``string``\ 의 타입을 가집니다.
-  takes ``time``: one of {“before”, “after”}
-  ``object``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.

트랜잭션 전 또는 후의 read 값.

속성에서만 지원됩니다.

.. _Authorization:

인증 연산자
-----------

.. _FAuthorizedBy:

authorized-by
~~~~~~~~~~~~~

.. code:: lisp

    (authorized-by k)

-  ``k``\ 는\ ``string``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.

지정된 이름의 키셋이 함수에서 적용되는지의 여부.

속성에서만 지원됩니다.

.. _FRowEnforced:

row-enforced
~~~~~~~~~~~~

.. code:: lisp

    (row-enforced t c r)

-  ``t``\ 는 *a* 의 타입을 가집니다.
-  ``c``\ 는 *b* 의 타입을 가집니다.
-  ``r``\ 은 ``string``\ 의 타입을 가집니다.
-  ``bool``\ 의 타입을 가진 값을 산출합니다.
-  이때 *a* 는 ``table`` 또는 ``string``\ 의 타입을 가집니다.
-  이때 *b* 는 ``column`` 또는 ``string``\ 의 타입을 가집니다.

지정된 행의 키셋이 함수에서 enforce 되는지의 여부.

속성에서만 지원됩니다.

.. |image0| image:: img/kadena-logo-210px.png
.. |image1| image:: img/kadena-logo-210px.png
