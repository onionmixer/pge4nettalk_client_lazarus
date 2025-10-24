# pge4nettalk_client_lazarus

BlahBlah 클라이언트 애플리케이션 (Lazarus/Free Pascal)

## 빌드 방법

이 프로젝트는 Free Pascal Compiler(fpc)와 fpcmake 빌드 시스템을 사용합니다.

### 사전 요구사항

- Free Pascal Compiler (fpc)
- Lazarus IDE (선택사항)
- fpcmake

### 빌드 명령어

빌드 스크립트를 사용하여 release 또는 debug 버전을 빌드할 수 있습니다:

```bash
# Release 빌드 (최적화, 디버그 메시지 없음)
./build.sh release

# Debug 빌드 (디버그 메시지 출력, core dump 지원)
./build.sh debug

# 빌드 아티팩트 정리
./build.sh clean

# 도움말 표시
./build.sh help
```

### 빌드 출력 위치

- **Release 빌드**: `build/release/pge4nettalk_client_lazarus`
- **Debug 빌드**: `build/debug/pge4nettalk_client_lazarus`

### Debug 빌드 특징

Debug 빌드는 다음과 같은 특징을 가집니다:

1. **디버그 메시지 출력**: `[timestamp][DEBUG]` 형식의 상세한 디버그 로그 출력
2. **Core dump 지원**: 크래시 발생 시 core dump 파일 생성 가능
3. **디버그 심볼 포함**: 디버깅 도구(gdb 등)에서 사용 가능한 심볼 정보 포함

Core dump를 사용하려면 다음 명령어를 실행하세요:

```bash
ulimit -c unlimited
```

### 로깅 시스템

애플리케이션은 다음과 같은 로깅 형식을 사용합니다:

- **일반 메시지**: `[yyyy-mm-dd hh:nn:ss][INFO] message`
- **디버그 메시지** (debug 빌드만): `[yyyy-mm-dd hh:nn:ss][DEBUG] message`

모든 로그는 표준 출력(stdout)으로 출력됩니다.

## 프로젝트 구조

- `*.pas`, `*.lpr`: Pascal 소스 파일
- `logger.pas`: 로깅 유틸리티 모듈
- `ozapi_pascal/`: ozapi 서브모듈
- `fundamentals/`: Fundamentals 라이브러리
- `lazrichview/`: RichView 컴포넌트
- `build.sh`: 빌드 스크립트
- `Makefile.fpc`: fpcmake 설정 파일

## 라이선스

[프로젝트 라이선스 정보]
